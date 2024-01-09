{-
  Syntax for types and helper functions
  Pedro Vasconcelos, 2023
-}

module Types exposing (Type(..), Kind(..),  Tycon, Tyvar, TySubst, -- TyAliases, 
                       tyBool, tyInt, tyChar, tyOrdering, tyString,
                       tyConst, applyTySubst, generalize,
                       freeTyVars, genVars)

import Dict exposing (Dict)
import Set exposing (Set)

type alias Tyvar
    = String

type alias Tycon
    = String
      
-- syntax for types
type Type
    = TyVar Tyvar                -- free type variable
    | TyGen Int                  -- quantified (generic) type variable
    | TyConst Tycon (List Type)  -- type constructor applied to type arguments
    | TyTuple (List Type)        -- special type constructors
    | TyList Type
    | TyFun Type Type

-- syntax for kinds
type Kind
    = KindStar
    | KindFun Kind Kind
      
-- type substitutions
type alias TySubst
    = Dict Tyvar Type


tyBool : Type
tyBool = TyConst "Bool" []

tyInt : Type         
tyInt = TyConst "Int" []

tyChar : Type
tyChar = TyConst "Char" []

tyOrdering : Type
tyOrdering = TyConst "Ordering" []

tyString : Type
tyString = TyList tyChar

-- smart constructor for type constants
-- special case for Strings
tyConst : Tycon -> List Type -> Type
tyConst c ts
    = case c of
          "String" ->
              if List.isEmpty ts then 
                  tyString
              else
                  TyConst c ts
          _ ->
              TyConst c ts
                     
           
-- apply a type substitution
applyTySubst : TySubst -> Type -> Type
applyTySubst s ty
    = case ty of
          TyGen _ ->
              ty
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
          TyConst c ts
              -> TyConst c (List.map (applyTySubst s) ts)

                 
-- quantify free variable of a type
-- first argument are the free variables in the type environment
generalize : Set Tyvar -> Type -> Type
generalize fvs ty
    = let vs = List.filter (\y -> not (Set.member y fvs)) (freeTyVars ty) 
          gs = List.range 0 (List.length vs - 1)
          s = Dict.fromList <| List.map2 (\v i -> Tuple.pair v (TyGen i)) vs gs
      in applyTySubst s ty

          
-- set of all free type variables in a type, no repeat entries
freeTyVars : Type -> List Tyvar
freeTyVars ty
    = nub (freeTyVarsAux ty)

-- worker function      
freeTyVarsAux : Type -> List Tyvar
freeTyVarsAux ty
    = case ty of
          TyGen _ ->
              []
          TyVar v ->
              [v]
          TyList t1 ->
              freeTyVarsAux t1
          TyTuple ts ->
              List.concatMap freeTyVarsAux ts
          TyFun t1 t2 ->
              freeTyVarsAux t1 ++ freeTyVarsAux t2
          TyConst c ts ->
              List.concatMap freeTyVarsAux ts

-- set of all generic vars in a type, no repeated entries
genVars : Type -> List Int
genVars ty
    = nub (genVarsAux ty)

-- worker function
genVarsAux : Type -> List Int
genVarsAux ty
    = case ty of
          TyVar _ ->
              []
          TyGen n ->
              [n]
          TyList t1 ->
              genVarsAux t1
          TyTuple ts ->
              List.concatMap genVarsAux ts
          TyFun t1 t2 ->
              genVarsAux t1 ++ genVarsAux t2
          TyConst c ts ->
              List.concatMap genVarsAux ts

-- remove duplicate entries
nub : List a -> List a
nub lst
    = nubAux lst []

-- worker function
nubAux : List a -> List a -> List a
nubAux lst acc
    = case lst of
          [] ->
              List.reverse acc
          (x :: xs) ->
              if List.member x acc then
                  nubAux xs acc
              else
                  nubAux xs (x::acc)



                      
