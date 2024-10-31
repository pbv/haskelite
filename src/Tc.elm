{-
  A monad for type checking and inference
  A combination of state (fresh variable generation and current unifier)
  and failure (type errors).

  Pedro Vasconcelos, 2022-24
-}
module Tc exposing (Tc, run, eval, pure, fail, context, andThen,
                    get, put, modify, traverse, traverse_, simplify,
                    freshType, freshTypes, freshVar, freshVars, freshInst, unify)

import Dict exposing (Dict)
import Set
import Types exposing (Type(..), TySubst, Tyvar, applyTySubst)
import State exposing (State)
import Unify

type Tc a
    = Tc (State TcState (Result Error a))

type alias TcState
    = { varcount : Int          -- current fresh variable counter
      , unifier : TySubst       -- current most general unifier
      }

-- an error with a possible context
type alias Error = { context : List String
                   , reason : String
                   }


errorToString : Error -> String
errorToString err
    = if List.isEmpty err.context then
          err.reason
      else
          String.join ", " err.context ++ ": " ++ err.reason
      
fromTc : Tc a -> State TcState (Result Error a)
fromTc (Tc m)
    = m

run : TcState -> Tc a -> (Result String a, TcState)
run s m
    = case State.run s (fromTc m) of
          (r, st) -> (Result.mapError errorToString r, st)

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
                 Err err -> State.state (Err err)
      ) (fromTc m))

-- fail with an error message
fail : String -> Tc a
fail msg
    = Tc (State.state <| Err { context=[], reason=msg })

-- add a context to errors
context : String -> Tc a -> Tc a
context msg action
    = Tc <| State.map (Result.mapError (\err -> { err | context = msg::err.context })) (fromTc action)
          
         
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

freshVar : Tc Tyvar
freshVar = get |>
            andThen
            (\s -> let c = s.varcount
                   in put { s | varcount = 1 + c }
                   |> andThen (\_ -> pure (mkVar c)))
      
freshVars : Int -> Tc (List Tyvar)
freshVars n
    = get |>
      andThen
      (\s -> let c = s.varcount
             in put { s | varcount = n + c }
             |> andThen (\_ -> pure (List.map mkVar (List.range c (n+c-1)))))
                
freshType : Tc Type
freshType
    = freshVar |> andThen (\v -> pure (TyVar v))

freshTypes : Int -> Tc (List Type)
freshTypes n
    = freshVars n |>
      andThen (\vs -> pure (List.map TyVar vs))
      
     
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

          TyConst c ts ->
            TyConst c (List.map (freshInst_ r) ts)

          TyVar _ ->
              ty
       
                          
mkVar : Int -> String
mkVar n
    = "a" ++ String.fromInt n

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


        
          
