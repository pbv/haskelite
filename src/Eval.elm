
module Eval exposing (..)

import AST exposing (Expr(..), Pattern(..), Decl(..), Name, Subst)
import Dict exposing (Dict)


-- * semantics
-- function definitions
type alias Functions
    = Dict AST.Name Function

-- the semantics of a single definition is a function from the stack
-- of arguments after unwinding to an optional expression; a result of
-- Nothing means that the function cannot be applied, possibly
-- requiring more evaluation of the arguments
type alias Function
    = List Expr -> Maybe Expr

-- alternatives equations for a single function
type alias Alt 
    = (List Pattern, Expr)

    
-- built-in operations
primitives : Functions
primitives
    = Dict.fromList
      [ ("+", arithOp (+))
      , ("-", arithOp (-))
      , ("*", arithOp (*))
      , ("div", arithOp (//))
      , ("mod", arithOp (\x y -> modBy y x))
      , ("==", equals)
      ]

arithOp : (Int -> Int -> Int) -> List Expr -> Maybe Expr    
arithOp func args
    = case args of
          [Number x, Number y] ->
              Just (AST.Number (func x y))
          _ -> if List.length args > 2 then
                   Just (Fail "type error")
               else 
                   Nothing

-- simple equality for numbers only
-- TODO: extend this to lists
equals : List Expr -> Maybe Expr
equals args
    = case args of
          [Number x, Number y] ->
              Just (Boolean (x==y))
          _ -> if List.length args > 2 then
                   Just (Fail "type error")
               else
                   Nothing


                       
-- check if an expression is a normal form
normalForm : Expr -> Bool
normalForm expr
    = case expr of
          Number _ -> True
          Boolean _ -> True
          ListLit xs -> List.all normalForm xs
          _ -> False
      

-- transform a list of declarations into a dictionary for functions
collectFunctions : List Decl -> Functions -> Functions
collectFunctions decls accum
    = case decls of
          [] ->
              accum
          (TypeSig _ _ :: rest) ->
              collectFunctions rest accum
          (Equation fun ps e :: rest) ->
              let
                  (alts1,rest1) = collectAlts fun rest
                  semantic = dispatchAlts ((ps,e)::alts1)
                  accum1 = Dict.insert fun semantic accum
              in collectFunctions rest1 accum1
                  
                  

-- collect all contiguous equations for a given name
collectAlts : Name -> List Decl -> (List Alt, List Decl)
collectAlts fun decls
    = case decls of
          [] -> ([], [])
          (TypeSig _ _ :: rest) ->
              ([], rest)
          (Equation f ps e :: rest) ->
              if f==fun then
                  let (alts, rest1) = collectAlts fun rest
                  in ((ps,e)::alts, rest1)
              else
                  ([], decls)

-- apply a function specifified by a list of alterantives
-- to a list of arguments
-- result is Nothing if the expression can't be reduced yet
dispatchAlts : List Alt -> List Expr -> Maybe Expr
dispatchAlts alts args
    = case alts of
          [] -> Just (Fail "pattern match failure")
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
                                Nothing -> dispatchAlts alts1 args
                                Just s -> Just (applyArgs (AST.applySubst s e) args2)
    

-- perform a single step reduction    
redex : Functions -> Expr -> Maybe Expr
redex functions expr =
    case expr of
        App e1 es ->
            case unwindArgs e1 es of
                (Lam xs e0, args) ->
                    let
                        alt = (List.map VarP xs, e0)
                    in 
                        dispatchAlts [alt] args
                            
                (Var fun, args) ->
                    case Dict.get fun functions of
                        Just semantics ->
                            semantics args
                        Nothing ->
                            Just (Fail "invalid function")
                _ -> Just (Fail "invalid function")
                    
        Cons e1 (ListLit l) ->
            Just <| ListLit (e1::l)
                
        InfixOp op e1 e2 ->
            redex functions (App (Var op) [e1, e2])

        IfThenElse e1 e2 e3 ->
            case e1 of
                Boolean True -> Just e2
                Boolean False -> Just e3
                _ -> Nothing
                     
        _ -> Nothing

             

-- unwind nested applications in a stack of arguments             
unwindArgs : Expr -> List Expr -> (Expr, List Expr)                       
unwindArgs e args 
    = case e of
          (App e1 es) -> unwindArgs e1 (es++args)
          _ -> (e, args)

-- reverse the unwound stack of arguments back into an application
applyArgs : Expr -> List Expr -> Expr
applyArgs e0 args
    = case args of
          [] -> e0
          _ -> App e0 args

               

-- check if a pattern must force evaluation of an expression
-- to perform maching
forceEval : Pattern -> Expr -> Bool
forceEval p e =
    case (p, e) of
        (VarP _, _) ->           False
        (NumberP _, Number _) -> False
        (BooleanP _, Boolean _) -> False
        (NilP,     ListLit _) -> False
        (NilP,    Cons _ _)   -> False
        (ConsP p1 p2, Cons e1 e2) ->
            forceEval p1 e1 || forceEval p2 e2
        (ConsP p1 p2, ListLit [])  -> False
        (ConsP p1 p2, ListLit (e1::rest))  ->
            forceEval p1 e1 || forceEval p2 (AST.ListLit rest)
        (_, _) -> True

forceEvalList : List Pattern -> List Expr -> Bool
forceEvalList ps es = List.any identity <| List.map2 forceEval ps es
                  

-- perform pattern matching
matching : Pattern -> Expr -> Subst -> Maybe Subst
matching p e s
    = case p of
          (VarP x) ->
              Just (Dict.insert x e s)
                  
          (NumberP n) ->
              case e of
                  Number m -> if n==m then Just s else Nothing
                  _        -> Nothing
                              
          (BooleanP b) ->
              case e of
                  Boolean c -> if b==c then Just s else Nothing
                  _         -> Nothing
                               
          NilP ->
              case e of
                  ListLit [] -> Just s
                  _          -> Nothing
                                
          (ConsP p1 p2) ->
              case e of
                  (Cons e1 e2) -> matching p1 e1 s
                                   |> Maybe.andThen (matching p2 e2)
                  (ListLit (e1::e2)) -> matching p1 e1 s
                                   |> Maybe.andThen (matching p2 (ListLit e2))
                  _            -> Nothing
                  

matchingList : List Pattern -> List Expr -> Subst -> Maybe Subst
matchingList ps es s
    = case (ps, es) of
          (p1::ps1, e1::es1) -> matching p1 e1 s
                               |> Maybe.andThen (\s1 -> matchingList ps1 es1 s1)
          (_, _) -> Just s

