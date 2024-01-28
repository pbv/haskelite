{-
  Helper functions to implement the heap
  Pedro Vasconcelos, 2023
-}
module Machine.Heap exposing (..)

import AST exposing (Expr(..), Name, Bind, Subst)
import Dict exposing (Dict)

type alias Heap 
    = { store : Dict Name Expr   -- mapping from name to expressions
      , counter : Int            -- for generating new names
      }

empty : Heap
empty =
    { store = Dict.empty
    , counter = 0
    }

get : Name -> Heap -> Maybe Expr
get name heap =
    Dict.get name heap.store

fromList : List (Name,Expr) -> Heap
fromList pairs
    = { store = Dict.fromList pairs
      , counter = 0
      }

insertFromList : Heap -> List (Name,Expr) -> Heap
insertFromList 
    = List.foldr (\(name,expr) heap -> update name expr heap)  
    
fromBinds : List Bind -> Heap
fromBinds binds
    = { store = Dict.fromList (List.map (\b -> (b.name, b.expr)) binds)
      , counter = 0
      }
      
update : Name -> Expr -> Heap -> Heap
update name newExpr heap =
    { heap | store = Dict.insert name newExpr heap.store }



-- remove suffix from location to get the source code name
getName : Name -> Name
getName loc
    = case String.split "$" loc of
          [] ->
              loc
          (prefix::_) ->
              prefix
                   
isIndirection : Name -> Bool
isIndirection =
    String.startsWith "$"

        
        
-- a new indirection for a non-recursive binding
-- introduced to implement lazy evaluatiom
newIndirection : Heap -> Expr -> (Name, Heap)
newIndirection heap expr
    = let
        loc = String.append "$" (String.fromInt heap.counter)
        newHeap = { store = Dict.insert loc expr heap.store
                  , counter = 1 + heap.counter
                  }
      in
          (loc, newHeap)        


-- allocate new possibly-recursive list of bindings
newBindings : Heap -> List Bind -> (Subst, Heap)
newBindings heap binds
    = let
        names = List.map .name binds
        exprs = List.map .expr binds
        suffix = String.fromInt heap.counter
        locs =  List.map (\x -> x ++ "$" ++ suffix) names
        subst = Dict.fromList <| List.map2 (\name loc -> (name,Var loc)) names locs
        store1 = List.foldl
                 (\(loc,expr) -> Dict.insert loc (AST.applySubst subst expr))
                 heap.store 
                    <| List.map2 Tuple.pair locs exprs
        newHeap = { store = store1
                  , counter = 1 + heap.counter
                  }
      in
          (subst, newHeap)
