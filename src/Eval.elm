
module Eval exposing (..)

import AST exposing (Expr, Pattern, Decl, Subst)
import Dict exposing (Dict)


-- * semantics
-- function definitions
type alias Functions
    = Dict AST.Name Function

type alias Function
    = List Alt

type alias Alt 
    = (List Pattern, Expr)

type alias Primitives
    = Dict AST.Name Prim
      
-- primitive binary operations
type alias Prim
    = List Expr -> Maybe Expr
      
-- built-in operations
primitives : Primitives
primitives
    = Dict.fromList
      [ ("+", arithOp (+))
      , ("-", arithOp (-))
      , ("*", arithOp (*))
      , ("div", arithOp (//))
      , ("mod", arithOp (\x y -> modBy y x))
      , ("==", polyEquals)
      ]

arithOp : (Int -> Int -> Int) -> List Expr -> Maybe Expr    
arithOp func args
    = case args of
          [AST.Number x, AST.Number y] ->
              Just (AST.Number (func x y))
          _ -> if List.length args > 2 then
                   Just (AST.Fail "type error")
               else 
                   Nothing

-- polymorphic equality
polyEquals : List Expr -> Maybe Expr
polyEquals args
    = case args of
          [AST.Number x, AST.Number y] ->
              Just (AST.Boolean (x==y))
          _ -> if List.length args > 2 then
                   Just (AST.Fail "type error")
               else
                   Nothing


                       
-- check if an expression is a normal form
normalForm : Expr -> Bool
normalForm expr
    = case expr of
          AST.Number _ -> True
          AST.Boolean _ -> True
          AST.ListLit xs -> List.all normalForm xs
          _ -> False
      

-- transform a list of declarations into a dictionary for functions
collectFunctions : List Decl -> Functions
collectFunctions decls
    = case decls of
          [] ->
              Dict.empty
          (AST.TypeSig _ _ :: rest) ->
              collectFunctions rest
          (AST.Equation f ps e :: rest) ->
              let
                  (alts1,rest1) = collectAlts f rest
                  functions = collectFunctions rest1
              in
                  Dict.insert f ((ps,e)::alts1) functions
                  

-- collect all contiguous equations for a given name
collectAlts : AST.Name -> List Decl -> (List Alt, List Decl)
collectAlts fun decls
    = case decls of
          [] -> ([], [])
          (AST.TypeSig _ _ :: rest) ->
              ([], rest)
          (AST.Equation f ps e :: rest) ->
              if f==fun then
                  let (alts, rest1) = collectAlts fun rest
                  in ((ps,e)::alts, rest1)
              else
                  ([], decls)

-- apply a function to a list of arguments
-- result is Nothing if the expression can't be reduced yet
applyFun : Function -> List Expr -> Maybe AST.Expr
applyFun alts args
    = case alts of
          [] -> Just (AST.Fail "pattern match failure")
          ((ps,e)::alts1) ->
              let nps = List.length ps
                  nargs = List.length args
              in if nargs < nps
                 then Nothing
                 else
                     let args1 = List.take nps args
                         args2 = List.drop nps args
                     in if forceEvalList ps args1
                        then
                            Nothing
                        else
                            case matchingList ps args1 Dict.empty of
                                Nothing -> applyFun alts1 args
                                Just s -> Just (applyArgs (AST.applySubst s e) args2)
    

applyPrim : Prim -> List Expr -> Maybe AST.Expr
applyPrim prim args = prim args
      
findRedex : Functions -> AST.Expr -> Maybe AST.Expr
findRedex functions expr =
    case expr of
        AST.App e1 e2 ->
            case unwindArgs e1 [e2] of
                (AST.Lam x t, arg1::rest) ->
                    let s = Dict.singleton x arg1
                    in Just <| applyArgs (AST.applySubst s t) rest
                (AST.Var fun, args) ->
                    case Dict.get fun primitives of
                        Just op ->
                            applyPrim op args
                        Nothing ->
                            case Dict.get fun functions of
                                Just alts ->
                                    applyFun alts args
                                Nothing ->
                                    Just (AST.Fail "invalid function")
                _ -> Just (AST.Fail "invalid function")
                    
        AST.Cons e1 (AST.ListLit l) ->
            Just <| AST.ListLit (e1::l)
                
        AST.InfixOp op e1 e2 ->
            findRedex functions (AST.App (AST.App (AST.Var op) e1) e2)

        AST.IfThenElse e1 e2 e3 ->
            case e1 of
                AST.Boolean True -> Just e2
                AST.Boolean False -> Just e3
                _ -> Nothing
                     
        _ -> Nothing

             

             
unwindArgs : Expr -> List Expr -> (Expr, List Expr)                       
unwindArgs e args 
    = case e of
          (AST.App e1 e2) -> unwindArgs e1 (e2::args)
          _ -> (e, args)

applyArgs : Expr -> List Expr -> Expr               
applyArgs = List.foldl (\x y -> AST.App  y x)
                        



-- check if a pattern must force evaluation of an expression
-- to perform maching
forceEval : AST.Pattern -> AST.Expr -> Bool
forceEval p e =
    case (p, e) of
        (AST.VarP _, _) ->           False
        (AST.NumberP _, AST.Number _) -> False
        (AST.BooleanP _, AST.Boolean _) -> False
        (AST.NilP,     AST.ListLit _) -> False
        (AST.NilP,    AST.Cons _ _)   -> False
        (AST.ConsP p1 p2, AST.Cons e1 e2) ->
            forceEval p1 e1 || forceEval p2 e2
        (AST.ConsP p1 p2, AST.ListLit [])  -> False
        (AST.ConsP p1 p2, AST.ListLit (e1::rest))  ->
            forceEval p1 e1 || forceEval p2 (AST.ListLit rest)
        (_, _) -> True

forceEvalList : List AST.Pattern -> List AST.Expr -> Bool
forceEvalList ps es = List.any identity <| List.map2 forceEval ps es
                  

-- perform pattern matching
matching : Pattern -> Expr -> Subst -> Maybe Subst
matching p e s
    = case p of
          (AST.VarP x) ->
              Just (Dict.insert x e s)
                  
          (AST.NumberP n) ->
              case e of
                  AST.Number m -> if n==m then Just s else Nothing
                  _        -> Nothing
                              
          (AST.BooleanP b) ->
              case e of
                  AST.Boolean c -> if b==c then Just s else Nothing
                  _         -> Nothing
                               
          AST.NilP ->
              case e of
                  AST.ListLit [] -> Just s
                  _          -> Nothing
                                
          (AST.ConsP p1 p2) ->
              case e of
                  (AST.Cons e1 e2) -> matching p1 e1 s
                                   |> Maybe.andThen (matching p2 e2)
                  (AST.ListLit (e1::e2)) -> matching p1 e1 s
                                   |> Maybe.andThen (matching p2 (AST.ListLit e2))
                  _            -> Nothing
                  

matchingList : List Pattern -> List Expr -> Subst -> Maybe Subst
matchingList ps es s
    = case (ps, es) of
          (p1::ps1, e1::es1) -> matching p1 e1 s
                               |> Maybe.andThen (\s1 -> matchingList ps1 es1 s)
          (_, _) -> Just s

