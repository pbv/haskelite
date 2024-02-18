{-
  Syntax for types and helper functions
  Pedro Vasconcelos, 2023
-}

module Types exposing (Type(..), Kind(..), Tycon, Tyvar, TySubst,
                       tyBool, tyInt, tyChar, tyOrdering, tyList,
                       tyUnit, tyPair, tyTuple3, tyTuple4, tyString, tyConst,
                       applyTySubst, generalize, freeTyVars, genVars)

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
    | TyFun Type Type            -- functions

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

tyList : Type -> Type
tyList t = TyConst "[]" [t]

tyUnit : Type
tyUnit = TyConst "()" []
           
tyPair : Type -> Type -> Type
tyPair t1 t2 = TyConst "(,)" [t1,t2]

tyTuple3 : Type -> Type -> Type -> Type
tyTuple3 t1 t2 t3 = TyConst "(,,)" [t1,t2,t3]

tyTuple4 : Type -> Type -> Type -> Type -> Type 
tyTuple4 t1 t2 t3 t4 = TyConst "(,,,)" [t1,t2,t3,t4] 
               
tyString : Type
tyString = tyList tyChar

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
    = nub (freeTyVarsAux ty [])

-- worker function      
freeTyVarsAux : Type -> List Tyvar -> List Tyvar
freeTyVarsAux ty acc
    = case ty of
          TyGen _ ->
              acc
          TyVar v ->
              v::acc
          TyFun t1 t2 ->
              freeTyVarsAux t1 (freeTyVarsAux t2 acc)
          TyConst c ts ->
              List.foldr freeTyVarsAux acc ts

-- set of all generic vars in a type, no repeated entries
genVars : Type -> List Int
genVars ty
    = nub (genVarsAux ty [])

-- worker function
genVarsAux : Type -> List Int -> List Int
genVarsAux ty acc
    = case ty of
          TyVar _ ->
              acc
          TyGen n ->
              n::acc
          TyFun t1 t2 ->
              genVarsAux t1 (genVarsAux t2 acc)
          TyConst c ts ->
              List.foldr genVarsAux acc ts

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



                      
