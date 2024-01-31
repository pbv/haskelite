{-
  Helper functions to implement the heap
  Pedro Vasconcelos, 2023
-}
module Machine.Heap exposing (..)

import AST exposing (Expr(..), Name, Bind, Subst)
import Dict exposing (Dict)

type alias Heap 
    = { store : Dict Name Expr   -- mapping from name to expressions
      , indirections : Int       -- counter for indirections
      , bound : Int            -- counter for let/where bound variables
      }

empty : Heap
empty =
    { store = Dict.empty
    , indirections = 0
    , bound = 0
    }

get : Name -> Heap -> Maybe Expr
get name heap =
    Dict.get name heap.store

fromList : List (Name,Expr) -> Heap
fromList pairs
    = { store = Dict.fromList pairs
      , indirections = 0
      , bound = 0
      }

insertFromList : Heap -> List (Name,Expr) -> Heap
insertFromList 
    = List.foldr (\(name,expr) heap -> update name expr heap)  
    
fromBinds : List Bind -> Heap
fromBinds binds
    = { store = Dict.fromList (List.map (\b -> (b.name, b.expr)) binds)
      , bound = 0
      , indirections = 0
      }
      
update : Name -> Expr -> Heap -> Heap
update name newExpr heap =
    { heap | store = Dict.insert name newExpr heap.store }


isIndirection : Name -> Bool
isIndirection =
    String.startsWith "$"

        
        
-- a new indirection for a non-recursive binding
-- introduced to implement lazy evaluatiom
newIndirection : Heap -> Expr -> (Name, Heap)
newIndirection heap expr
    = let
        loc = String.append "$" (String.fromInt heap.indirections)
        newHeap = { store = Dict.insert loc expr heap.store
                  , indirections = 1 + heap.indirections
                  , bound = heap.bound
                  }
      in
          (loc, newHeap)        


-- allocate new possibly-recursive list of bindings
newBindings : Heap -> List Bind -> (Subst, Heap)
newBindings heap binds
    = let
        names = List.map .name binds
        exprs = List.map .expr binds
        suffix = String.fromInt heap.bound
        locs =  List.map (\x -> x ++ "_" ++ suffix) names
        subst = Dict.fromList <| List.map2 (\name loc -> (name,Var loc)) names locs
        store1 = List.foldl
                 (\(loc,expr) -> Dict.insert loc (AST.applySubst subst expr))
                 heap.store 
                    <| List.map2 Tuple.pair locs exprs
        newHeap = { store = store1
                  , bound = 1 + heap.bound
                  , indirections = heap.indirections
                  }
      in
          (subst, newHeap)
