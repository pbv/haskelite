{-
  Helper functions to implement the heap
  Pedro Vasconcelos, 2023
-}
module Machine.Heap exposing (..)

import AST exposing (Expr(..), Matching(..), Qual(..),
                         Pattern, Name, Bind, Subst, Info)
import Dict exposing (Dict)

type alias Heap 
    = { store : Dict Name Expr   -- mapping from name to expressions
      , indirections : Int       -- counter for indirections
      , bounds : Int             -- counter for let/where bound variables
      }

empty : Heap
empty =
    { store = Dict.empty
    , indirections = 0
    , bounds = 0
    }

get : Name -> Heap -> Maybe Expr
get name heap =
    Dict.get name heap.store

delete : Name -> Heap -> Heap
delete name heap =
    { heap | store = Dict.remove name heap.store }

size : Heap -> Int
size heap =
    Dict.size heap.store
        
fromList : List (Name,Expr) -> Heap
fromList pairs
    = { store = Dict.fromList pairs
      , indirections = 0
      , bounds = 0
      }

insertFromList : Heap -> List (Name,Expr) -> Heap
insertFromList 
    = List.foldr (\(name,expr) heap -> update name expr heap)  
    
fromBinds : List Bind -> Heap
fromBinds binds
    = { store = Dict.fromList (List.map (\b -> (b.name, b.expr)) binds)
      , bounds = 0
      , indirections = 0
      }
      
update : Name -> Expr -> Heap -> Heap
update name newExpr heap =
    { heap | store = Dict.insert name newExpr heap.store }


isIndirection : Name -> Bool
isIndirection =
    String.startsWith "$"

            
-- create a new indirection for argument normalization
newIndirection : Heap -> Expr -> (Name, Heap)
newIndirection heap expr
    = let
        loc = String.append "$" (String.fromInt heap.indirections)
        newHeap = { store = Dict.insert loc expr heap.store
                  , indirections = 1 + heap.indirections
                  , bounds = heap.bounds
                  }
      in
          (loc, newHeap)        


-- allocate new possibly-recursive list of bindings
newBindings : Heap -> List Bind -> (Subst, Heap)
newBindings heap binds
    = let
        names = List.map .name binds
        exprs = List.map .expr binds
        suffix = String.fromInt heap.bounds
        locs =  List.map (\x -> x ++ "$" ++ suffix) names
        subst = Dict.fromList (List.map2 Tuple.pair names locs)
        store1 = List.foldl
                    (\(loc,expr) -> Dict.insert loc (AST.applySubst subst expr))
                    heap.store 
                    (List.map2 Tuple.pair locs exprs)
        newHeap = { store = store1
                  , bounds = 1 + heap.bounds
                  , indirections = heap.indirections
                  }
      in
          (subst, newHeap)

-------------------------------------------------------
-- expand indirections 
---------------------------------------------------------
expandExpr : Heap -> Expr -> Expr
expandExpr heap expr
    = case expr of
          App e1 e2 ->
              App (expandExpr heap e1) (expandExpr heap e2)
          Lam arity optName match ->
              Lam arity optName (expandMatch heap match)
          Var x ->
              if isIndirection x then
                  case get x heap of
                      Nothing ->
                          expr
                      Just e1 ->
                          -- delete from heap to avoid infinite expansion
                          expandExpr (delete x heap) e1
              else
                  expr
          Case e1 alts ->
              Case (expandExpr heap e1) (expandAlts heap alts)
          IfThenElse e1 e2 e3 ->
              IfThenElse (expandExpr heap e1) (expandExpr heap e2) (expandExpr heap e3) 
          Let binds e1 ->
              Let (expandBinds heap binds) (expandExpr heap e1)
          ListComp e1 qs ->
              ListComp (expandExpr heap e1) (List.map (expandQual heap) qs)
          BinaryOp op e1 e2 ->
              BinaryOp op (expandExpr heap e1) (expandExpr heap e2)
          UnaryOp op e1 ->
              UnaryOp op (expandExpr heap e1)
          Cons _ tag args ->
              Cons False tag (List.map (expandExpr heap) args)
          Number _ ->
              expr
          Char _ ->
              expr
          Exception _ ->
              expr
          Unimplemented _ ->
              expr
          Marked e1 ->
              Marked (expandExpr heap e1)

expandQual : Heap -> Qual -> Qual
expandQual heap q
    = case q of
          Gen p e -> Gen p (expandExpr heap e)
          Guard b -> Guard (expandExpr heap b)
          LetQual x e -> LetQual x (expandExpr heap e)
                  
expandAlts : Heap -> List (Pattern, Expr, Info) -> List (Pattern,Expr,Info)
expandAlts heap alts
    = case alts of
          [] ->
              []
          ((pat,expr,info)::rest) ->
              (pat, expandExpr heap expr, info) :: expandAlts heap rest

expandBinds : Heap -> List Bind -> List Bind
expandBinds heap binds
    = case binds of
          [] -> []
          (bind::rest) ->
              {bind|expr=expandExpr heap bind.expr} ::
                  expandBinds heap rest
                  
                      
expandMatch : Heap -> Matching -> Matching
expandMatch heap match
    = case match of
          Return e optInfo ->
              Return (expandExpr heap e) optInfo
          Fail ->
              match
          Match pat cont ->
              Match pat (expandMatch heap cont)
          Arg e cont ->
              Arg (expandExpr heap e) (expandMatch heap cont)
          Alt m1 m2 ->
              Alt (expandMatch heap m1) (expandMatch heap m2)
          Where binds cont ->
              Where (expandBinds heap binds) (expandMatch heap cont)
