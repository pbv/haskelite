{-
  A monad for type checking and inference
  A combination of state (fresh variable generation and 
  current unifier) and failure (for type errors).

  Pedro Vasconcelos, 2022-23
-}
module Tc exposing (Tc, run, eval, pure, fail, explain, andThen,
                   get, put, modify, traverse, simplify,
                   freshVar, freshVars, freshName, freshInst, unify)

import Dict exposing (Dict)
import Set
import Types exposing (Name, Type(..), TySubst, applyTySubst)
import State exposing (State)
import Unify
import Tuple


type Tc a
    = Tc (State TcState (Result String a))

type alias TcState
    = { varcount : Int          -- current fresh variable counter
      , unifier : TySubst       -- current most general unifier
      }

    
fromTc : Tc a -> State TcState (Result String a)
fromTc (Tc m)
    = m

run : TcState -> Tc a -> (Result String a, TcState)
run s m
    = State.run s (fromTc m) 

eval : Tc a -> Result String a
eval m
    = let s = { varcount = 0
              , unifier = Dict.empty
              }
      in Tuple.first (run s m)
          
pure : a -> Tc a
pure v
    = Tc (State.state <| Ok v)
         
andThen : (a -> Tc b) -> Tc a -> Tc b
andThen f m
    = Tc <|
      (State.andThen 
      (\r -> case r of
                 Ok v -> fromTc (f v)
                 Err e -> State.state (Err e)
      ) (fromTc m))


fail : String -> Tc a
fail e
    = Tc (State.state <| Err e)

-- add an explaining string to errors
explain : String -> Tc a -> Tc a
explain mesg action
    = Tc <| State.map (Result.mapError (\s ->  mesg ++ s)) (fromTc action)
          
         
get : Tc TcState
get = Tc <| State.map Ok State.get

put : TcState -> Tc ()
put s = Tc <| State.map Ok (State.put s)

modify : (TcState -> TcState) -> Tc ()
modify f = get |> andThen (\s -> put (f s))

-- traverse, aka mapM
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

freshVars : Int -> Tc (List Type)
freshVars n
    = if n<=0 then
          pure []
      else
          freshVar |> andThen (\v -> freshVars (n-1) |>
                                   andThen (\vs -> pure (v::vs)))
      
freshName : Tc Name
freshName = get |>
            andThen
            (\s -> let c = s.varcount
                   in put { s | varcount = 1 + c }
                   |> andThen (\_ -> pure (mkVar c)))



-- create fresh instances of generic variables in a type
freshInst : Type -> Tc Type
freshInst ty
    = let
        gvs = Set.toList (Types.genVars ty)
      in
          freshVars (List.length gvs) |>
          andThen (\vs -> let r = Dict.fromList <| List.map2 Tuple.pair gvs vs
                          in freshInst_ r ty)

-- worker function 
freshInst_ : Dict Int Type -> Type -> Tc Type
freshInst_ r ty
    = case ty of
          TyGen gv -> 
              pure <|
                  case Dict.get gv r of
                  Just t -> t
                  Nothing -> TyGen gv  -- NB: this should not happen!
          TyFun t1 t2 ->
              freshInst_ r t1 |>
              andThen (\t1n ->
              freshInst_ r t2 |>
              andThen (\t2n ->
              pure (TyFun t1n t2n)))
          TyList t1 ->
              freshInst_ r t1 |>
              andThen (\t1n -> pure (TyList t1n))
          TyTuple ts ->
              traverse (freshInst_ r) ts |>
              andThen (\nts -> pure (TyTuple nts))
          _ ->
            pure ty


          
                          
mkVar : Int -> String
mkVar n
    = "t" ++ String.fromInt n


unify : Type -> Type -> Tc ()
unify t1 t2
    = get |>
      andThen
      (\s -> case Unify.unifyEqs s.unifier [(t1,t2)] of
                 Ok r -> put { s | unifier=r }
                 Err e -> fail e)   



              
-- apply current substitution
simplify : Type -> Tc Type
simplify ty
    = get |>
      andThen (\s -> pure (applyTySubst s.unifier ty))


        
          
