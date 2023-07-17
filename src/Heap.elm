{-
  Helper functions to implement the heap
  Pedro Vasconcelos, 2023
-}
module Heap exposing (..)

import AST exposing (Expr(..), Name, Bind, Subst)
import Dict exposing (Dict)

type alias Heap 
    = Dict Name Expr

empty : Heap
empty = Dict.empty

get : Name -> Heap -> Maybe Expr
get = Dict.get

fromList : List (Name,Expr) -> Heap
fromList = Dict.fromList

fromBinds : List Bind -> Heap
fromBinds binds = Dict.fromList (List.map (\b -> (b.name, b.expr)) binds)
      
update : Name -> Expr -> Heap -> Heap
update = Dict.insert
      
isIndirection : Name -> Bool
isIndirection = String.startsWith "$"

-- a new indirection for a non-recursive binding
-- introduced to implement lazy evaluatiom
newIndirection : Heap -> Expr -> (Name, Heap)
newIndirection heap expr
    = let
        size = Dict.size heap
        loc = String.append "$" (String.fromInt size)
      in
          (loc, Dict.insert loc expr heap)         


-- allocate a new possibly-recursive set of bindings
newBindings : Heap -> List Bind -> (Subst, Heap)
newBindings heap0 binds
    = let
        names = List.map .name binds
        exprs = List.map .expr binds
        size = String.fromInt (Dict.size heap0)
        locs =  List.map (\x -> x ++ "_" ++ size) names
        subst = Dict.fromList <|
                List.map2 (\name loc -> (name,Var loc)) names locs
        heap1 = Dict.fromList <|
                List.map2 (\loc expr -> (loc, AST.applySubst subst expr)) locs exprs
      in
          (subst, Dict.union heap0 heap1)
