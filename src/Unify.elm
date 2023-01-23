
module Unify exposing (unifyEqs)

import Dict
import AST exposing (Name, Type(..), TySubst, applyTySubst)

unifyEqs : TySubst -> List (Type,Type) -> Result String TySubst
unifyEqs s eqs
    = case eqs of
          [] ->
              Ok s
          ((t1,t2)::rest) ->
              unifyAux s (applyTySubst s t1) (applyTySubst s t2) rest

unifyAux : TySubst -> Type -> Type -> List (Type,Type) -> Result String TySubst
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
                  Err "occurs check failed"
              else
                  unifyEqs (extend x t2 s) eqs
                        
          (TyInt, TyInt) ->
              unifyEqs s eqs
                  
          (TyBool, TyBool) ->
              unifyEqs s eqs
                  
          (TyList t3, TyList t4) ->
              unifyEqs s ((t3,t4)::eqs)
             
          (TyFun t1a t1b, TyFun t2a t2b) ->
              unifyEqs s ((t1a,t2a)::(t1b,t2b)::eqs)

          (_, _) ->
              Err "type mismatch"

extend : Name -> Type -> TySubst -> TySubst
extend v t s
    = let s1 = Dict.singleton v t
      in Dict.insert v t <| Dict.map (\_ -> applyTySubst s1) s


occurs : Name -> Type -> Bool
occurs v ty
    = case ty of
          TyVar x ->
              x == v
          TyFun t1 t2 ->
              occurs v t1 || occurs v t2
          TyList t1 ->
              occurs v t1
          TyTuple ts ->
              List.any (occurs v) ts
          _ ->
              False
                  
