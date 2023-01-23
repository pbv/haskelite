{-
  A combined State and Error monad for typechecking
-}
module Tc exposing (Tc, run, pure, fail, andThen,
                   get, put, modify, traverse,
                   freshVar, freshName, freshInst, unify)

import Dict exposing (Dict)
import AST exposing (Name, Type(..), TySubst)
import State exposing (State)
import Unify

type Tc a
    = Tc (State TcState (Result String a))

type alias TcState
    = { varcount : Int               -- current fresh variable counter
      , unifier : TySubst            -- current most general unifier
      , renaming : Dict Name Name    -- cache for fresh instance of QVars
      }

fromTc : Tc a -> State TcState (Result String a)
fromTc (Tc m) = m

run : TcState -> Tc a -> (Result String a, TcState)
run s m = State.run s (fromTc m) 
                
pure : a -> Tc a
pure v = Tc (State.state <| Ok v)
         
andThen : (a -> Tc b) -> Tc a -> Tc b
andThen f m
    = Tc <|
      (State.andThen 
      (\r -> case r of
                 Ok v -> fromTc (f v)
                 Err e -> fromTc (fail e)
      ) (fromTc m))


fail : String -> Tc a
fail e = Tc (State.state <| Err e)
    
get : Tc TcState
get = Tc <| State.map Ok State.get

put : TcState -> Tc ()
put s = Tc <| State.map Ok (State.put s)

modify : (TcState -> TcState) -> Tc ()
modify f = get |> andThen (\s -> put (f s))


traverse : (a -> Tc b) -> List a -> Tc (List b)
traverse f lst
    = case lst of
          [] -> pure []
          (v::vs) -> f v |> andThen
                     (\u -> traverse f vs |>
                            andThen(\us -> pure (u::us)))

freshVar : Tc Type
freshVar
    = freshName |> andThen (\n -> pure (TyVar n))
        
freshName : Tc Name
freshName = get |>
            andThen
            (\s -> let c = s.varcount
                   in put { s | varcount = 1 + c }
                   |> andThen (\_ -> pure (mkVar c)))


resetInst : Tc ()
resetInst = modify (\s -> {s | renaming = Dict.empty})                

-- wrapper
freshInst : Type -> Tc Type
freshInst ty = resetInst |> andThen (\_ -> freshInst_ ty)

-- worker function using the renaming cache
freshInst_ : Type -> Tc Type
freshInst_ ty
    = case ty of
          TyQVar qv -> 
              freshNameInst qv |>
              andThen (\n -> pure (TyVar n))
          TyFun t1 t2 ->
              freshInst_ t1 |>
              andThen (\t1n ->
              freshInst_ t2 |>
              andThen (\t2n ->
              pure (TyFun t1n t2n)))
          TyList t1 ->
              freshInst_ t1 |>
              andThen (\t1n -> pure (TyList t1n))
          TyTuple ts ->
              traverse freshInst_ ts |>
              andThen (\nts -> pure (TyTuple nts))
          _ ->
            pure ty
               

            
freshNameInst : Name -> Tc Name
freshNameInst qv = get |>
               andThen
               (\s -> case Dict.get qv s.renaming of
                          Just v -> pure v
                          Nothing -> let c = s.varcount
                                         v = mkVar c
                                     in
                                         put { s | varcount = 1 + c
                                             , renaming = Dict.insert qv v s.renaming
                                             }
                                         |> andThen (\_ -> pure v))
                                               
                
mkVar : Int -> String
mkVar n = "_t" ++ String.fromInt n


unify : Type -> Type -> Tc ()
unify t1 t2
    = get |>
      andThen
      (\s -> case Unify.unifyEqs s.unifier [(t1,t2)] of
                 Ok r -> put { s | unifier=r }
                 Err e -> fail e)
