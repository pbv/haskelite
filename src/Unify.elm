{- 
  Hindley Milner Type unification algorithm
  Pedro Vasconcelos, 2021-23
-}
module Unify exposing (..)

import Dict
import Types exposing (Name, Type(..), TySubst, applyTySubst)
import Pretty

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
                 occurCheckFail t1 t2
              else
                  unifyEqs (extend x t2 s) eqs
                        
          (TyInt, TyInt) ->
              unifyEqs s eqs
                  
          (TyBool, TyBool) ->
              unifyEqs s eqs
                  
          (TyList t3, TyList t4) ->
              unifyAux s t3 t4 eqs
             
          (TyTuple ts1, TyTuple ts2) ->
              if List.length ts1 == List.length ts2 then
                  unifyEqs s (List.map2 Tuple.pair ts1 ts2 ++ eqs)
              else
                  mismatch t1 t2
                  
          (TyFun t1a t1b, TyFun t2a t2b) ->
              unifyAux s t1a t2a ((t1b,t2b)::eqs)

          (_, _) ->
              mismatch t1 t2
                  

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



mismatch : Type -> Type -> Result String a
mismatch t1 t2
    = Err ("type mismatch: " ++
               Pretty.prettyType t1 ++ " and " ++
               Pretty.prettyType t2)
      

occurCheckFail : Type -> Type -> Result String a
occurCheckFail t1 t2
    = Err ("occur check fail (infinite type): " ++
               Pretty.prettyType t1 ++ " = " ++
               Pretty.prettyType t2 )
      
