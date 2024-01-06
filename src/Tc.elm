{-
  A monad for type checking and inference
  A combination of state (fresh variable generation and  current unifier)
  and failure (type errors).

  Pedro Vasconcelos, 2022-23
-}
module Tc exposing (Tc, run, eval, pure, fail, explain, andThen,
                    get, put, modify, traverse, traverse_, simplify,
                    freshType, freshTypes, freshVar, freshInst, unify)

import Dict exposing (Dict)
import Set
import Types exposing (Type(..), TySubst, Tyvar, applyTySubst)
import State exposing (State)
import Unify

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
          [] ->
              pure []
          (v::vs) ->
              f v |> andThen
                     (\u -> traverse f vs |>
                            andThen(\us -> pure (u::us)))

-- same as above, but ignore the results 
-- aka mapM_                          
traverse_ : (a -> Tc b) -> List a -> Tc ()
traverse_ f lst
    = case lst of
          [] ->
              pure ()
          (v::vs) ->
              f v |> andThen (\_ -> traverse_ f vs)

      
freshType : Tc Type
freshType
    = freshVar |> andThen (\v -> pure (TyVar v))

freshTypes : Int -> Tc (List Type)
freshTypes n
    = if n<=0 then
          pure []
      else
          freshType |> andThen (\v -> freshTypes (n-1) |>
                                    andThen (\vs -> pure (v::vs)))
      
freshVar : Tc Tyvar
freshVar = get |>
            andThen
            (\s -> let c = s.varcount
                   in put { s | varcount = 1 + c }
                   |> andThen (\_ -> pure (mkVar c)))



-- create fresh instances of generic variables in a type
freshInst : Type -> Tc Type
freshInst ty
    = let
        gvs = Types.genVars ty
      in
          freshTypes (List.length gvs) |>
          andThen (\vs -> let r = Dict.fromList <| List.map2 Tuple.pair gvs vs
                          in pure (freshInst_ r ty))

-- worker function to perform the generic variable replacement
freshInst_ : Dict Int Type -> Type -> Type
freshInst_ r ty
    = case ty of
          TyGen gv -> 
                  case Dict.get gv r of
                      Just t -> t
                      Nothing -> ty  -- NB: this should not happen!
          TyFun t1 t2 ->
              TyFun (freshInst_ r t1) (freshInst_ r t2)
                  
          TyList t1 ->
              TyList (freshInst_ r t1)

          TyTuple ts ->
              TyTuple (List.map (freshInst_ r) ts)

          TyConst c ts ->
            TyConst c (List.map (freshInst_ r) ts)

          TyVar _ ->
              ty


          
                          
mkVar : Int -> String
mkVar n
    = "t" ++ String.fromInt n


-- unify two types; the first argument is the pretty-printing function
unify : (Type -> String) -> Type -> Type -> Tc ()
unify show t1 t2
    = get |>
      andThen
      (\s -> case Unify.unifyEqs s.unifier [(t1,t2)] of
                 Ok r -> put { s | unifier=r }
                 Err (Unify.Mismatch t3 t4) ->
                        fail ("type mismatch: " ++ show t3 ++ " and " ++ show t4)
                 Err (Unify.OccursCheckFail t3 t4) ->
                        fail ("occurs check failed (infinite type): "++
                                  show t3 ++ " = " ++ show t4))
              
-- apply current substitution
simplify : Type -> Tc Type
simplify ty
    = get |>
      andThen (\s -> pure (applyTySubst s.unifier ty))


        
          
