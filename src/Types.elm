{-
  Syntax for types and helper functions
  Pedro Vasconcelos, 2023
-}

module Types exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)

type alias Name
    = String

type alias Tycon
    = String
      
-- syntax of types
type Type
    = TyVar Name         -- free type variable
    | TyGen Int          -- quantified (generic) type variable
    -- | TyBool
    -- | TyInt
    | TyConst Tycon      -- Bool, Int, Char
    | TyTuple (List Type)
    | TyList Type
    | TyFun Type Type

-- type substitutions
type alias TySubst
    = Dict Name Type

tyBool : Type
tyBool = TyConst "Bool"

tyInt : Type         
tyInt = TyConst "Int"

tyChar : Type
tyChar = TyConst "Char"

      
-- apply a type substitution
applyTySubst : TySubst -> Type -> Type
applyTySubst s ty
    = case ty of
          TyVar name ->
              case Dict.get name s of
                  Nothing -> ty
                  Just t1 -> t1
          TyList t1
              -> TyList (applyTySubst s t1)
          TyTuple ts
              -> TyTuple (List.map (applyTySubst s) ts)
          TyFun t1 t2
              -> TyFun (applyTySubst s t1) (applyTySubst s t2)
          _
              -> ty
      
-- quantify free variable of a type
-- first argument are the free variables in the type environment
generalize : Set Name -> Type -> Type
generalize fvs ty
    = let vs = Set.toList <| Set.diff (freeTyVars ty) fvs
          gs = List.range 0 (List.length vs - 1)
          s = Dict.fromList <| List.map2 (\v i -> Tuple.pair v (TyGen i)) vs gs
      in applyTySubst s ty
    
-- set of all free type variables in a type
freeTyVars : Type -> Set Name
freeTyVars ty
    = case ty of
          TyVar v ->
              Set.singleton v
          TyList t1 ->
              freeTyVars t1
          TyTuple ts ->
              List.foldl Set.union Set.empty <| List.map freeTyVars ts
          TyFun t1 t2 ->
              Set.union (freeTyVars t1) (freeTyVars t2)
          _ ->
              Set.empty

-- set of all generic vars in a type
genVars : Type -> Set Int
genVars ty
    = case ty of
          TyGen n ->
              Set.singleton n
          TyList t1 ->
              genVars t1
          TyTuple ts ->
              List.foldl Set.union Set.empty <| List.map genVars ts
          TyFun t1 t2 ->
              Set.union (genVars t1) (genVars t2)
          _ ->
              Set.empty
      

