{-
  Helper functions to implement the heap
  Pedro Vasconcelos, 2023
-}
module Heap exposing (..)

import AST exposing (Expr, Name)
import Dict exposing (Dict)

type alias Heap 
    = Dict Name Expr

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
