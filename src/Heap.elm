{-
  Helper functions to implement the heap
  Pedro Vasconcelos, 2023
-}
module Heap exposing (..)

import AST exposing (Expr, Name, Bind)
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
