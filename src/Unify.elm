{- 
  Hinley-Milner type unification algorithm
  Pedro Vasconcelos, 2021-23
-}
module Unify exposing (..)

import Dict
import Types exposing (Tyvar, Type(..), TySubst, applyTySubst)

type Error
    = Mismatch Type Type
    | OccursCheckFail Type Type
    

unifyEqs : TySubst -> List (Type,Type) -> Result Error TySubst
unifyEqs s eqs
    = case eqs of
          [] ->
              Ok s
          ((t1,t2)::rest) ->
              unifyAux s (applyTySubst s t1) (applyTySubst s t2) rest

unifyAux : TySubst -> Type -> Type -> List (Type,Type) -> Result Error TySubst
unifyAux s t1 t2 eqs
    = case (t1, t2) of
          (TyVar x, TyVar y) ->
              case compare x y of
                  EQ -> unifyEqs s eqs
                  LT -> unifyEqs (extend y (TyVar x) s) eqs
                  GT -> unifyEqs (extend x (TyVar y) s) eqs

          (_, TyVar _) ->
              unifyAux s t2 t1 eqs
                        
          (TyVar x, _) ->
              if occurs x t2 then
                  Err (OccursCheckFail t1 t2)
              else
                  unifyEqs (extend x t2 s) eqs
                        
          (TyConst c1 ts1, TyConst c2 ts2) ->
              if c1 == c2 then -- then length ts1 == length ts2
                  unifyEqs s (List.map2 Tuple.pair ts1 ts2 ++ eqs)
              else
                  Err (Mismatch t1 t2)                                    
             
          (TyFun t1a t1b, TyFun t2a t2b) ->
              unifyAux s t1a t2a ((t1b,t2b)::eqs)

          (_, _) ->
              Err (Mismatch t1 t2)
                  

extend : Tyvar -> Type -> TySubst -> TySubst
extend v t s
    = let s1 = Dict.singleton v t
      in Dict.insert v t <| Dict.map (\_ -> applyTySubst s1) s


occurs : Tyvar -> Type -> Bool
occurs v ty
    = case ty of
          TyGen _ ->
              False
          TyVar x ->
              x == v
          TyFun t1 t2 ->
              occurs v t1 || occurs v t2
          TyConst c ts ->
              List.any (occurs v) ts



