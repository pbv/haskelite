{-
  Single-step evaluation of Haskelite programs
  Pedro Vasconcelos, 2021
-} 
module Eval exposing (..)

import AST exposing (Expr(..), Pattern(..), Decl(..), Name, Subst)
import Pretty
import Dict exposing (Dict)
import Context exposing (Context)
import Monocle.Optional as Monocle


-- * semantics
-- function definitions
type alias Functions
    = Dict AST.Name Function

      
-- the semantics of a single definition is a function from the stack
-- of arguments after unwinding to an optional expression; a result of
-- Nothing means that the function cannot be applied, possibly
-- requiring more evaluation of the arguments
-- the result string is an explanation for the reducion 
type alias Function
    = List Expr -> Maybe (Expr,String)

-- alternatives equations for a single function
type alias Alt 
    = (List Pattern, Expr, String)


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
                  info = Pretty.prettyDecl (Equation fun ps e)
                  (alts1,rest1) = collectAlts fun rest
                  semantic = dispatchAlts ((ps,e,info)::alts1)
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
                  let
                      info = Pretty.prettyDecl (Equation f ps e)
                      (alts, rest1) = collectAlts fun rest
                  in ((ps,e,info)::alts, rest1)
              else
                  ([], decls)
    
    
-- built-in operations
primitives : Functions
primitives
    = Dict.fromList
      [ ("+", arithOp "+" (+))
      , ("-", arithOp "-" (-))
      , ("*", arithOp "*" (*))
      , ("div", arithOp "div" (//))
      , ("mod", arithOp "mod" (\x y -> modBy y x))
      , ("==", comparePoly "==" equalExpr)
      , ("/=", comparePoly "/=" (\x y -> not (equalExpr x y)))
      , (">=", compareOp ">=" (>=))
      , ("<=", compareOp "<=" (<=))
      , (">", compareOp ">" (>))
      , ("<", compareOp "<" (<))
      ]

arithOp : String -> (Int -> Int -> Int) -> List Expr -> Maybe (Expr,String)
arithOp op func args
    = case args of
          [Number x, Number y] ->
              Just (Number (func x y), op)
          [arg1, arg2] ->
              if isWeakNormalForm arg1 && isWeakNormalForm arg2 then
                  Just (Fail "type error: operator requires numbers", op)
              else
                  Nothing
          _ -> if List.length args > 2 then
                   Just (Fail "type error: wrong number of arguments", op)
               else 
                   Nothing

-- simple comparisons for numbers only
compareOp : String -> (Int -> Int -> Bool) -> List Expr -> Maybe (Expr,String)
compareOp op func args
    = case args of
          [Number x, Number y] ->
              Just (Boolean (func x y), op)
          _ -> if List.length args > 2 then
                   Just (Fail "type error: wrong number of arguments", op)
               else
                   Nothing


-- polymorphic comparisons
comparePoly : String -> (Expr -> Expr -> Bool) -> List Expr -> Maybe (Expr, String)
comparePoly op func args
    = case args of
          [arg1, arg2] ->
              if isNormalForm arg1 && isNormalForm arg2 then
                  Just (Boolean (func arg1 arg2), op)
              else
                  Nothing
          _ -> if List.length args > 2 then
                   Just (Fail "type error: wrong number of arguments", op)
               else
                   Nothing
                   

-- compare two expressions for equality; this is only nedeed because
-- Elm doesn't support "deriving Eq" on a data type definition ;-(
--
equalExpr : Expr -> Expr -> Bool
equalExpr expr1 expr2
    = case (expr1,expr2) of
          (App e1 args1, App e2 args2) ->
              equalExpr e1 e2 && equalList args1 args2
          (Lam xs1 e1, Lam xs2 e2) ->
              xs1 == xs2 && equalExpr e1 e2
          (Var x1, Var x2) ->
              x1 == x2
          (Number n1, Number n2) ->
              n1 == n2
          (Boolean b1, Boolean b2) ->
              b1 == b2
          (Cons e1 e2, Cons e3 e4) ->
              equalExpr e1 e3 && equalExpr e2 e4
          (ListLit items1, ListLit items2) ->
              equalList items1 items2
          (TupleLit items1, TupleLit items2) ->
              equalList items1 items2
          (InfixOp op1 e1 e2, InfixOp op2 e3 e4) ->
              op1 == op2 && equalExpr e1 e3 && equalExpr e2 e4
          (IfThenElse e1 e2 e3, IfThenElse e4 e5 e6) ->
              equalExpr e1 e4 && equalExpr e2 e5 && equalExpr e3 e6
          (Fail msg1, Fail msg2) ->
              msg1 == msg2
          _ ->
              False
              
equalList : List Expr -> List Expr -> Bool
equalList items1 items2 =
    List.all identity <| List.map2 equalExpr items1 items2
          
                       
                       
-- apply a function specifified by a list of alterantives
-- to a list of arguments
-- result is Nothing if the expression can't be reduced yet
dispatchAlts : List Alt -> List Expr -> Maybe (Expr,String)
dispatchAlts alts args
    = case alts of
          [] -> Just (Fail "pattern match failure", "error")
          ((ps,e,info)::alts1) ->
              let nps = List.length ps
                  nargs = List.length args
              in if nargs < nps
                 then Nothing
                 else
                     if forceEvalList ps args
                     then
                         Nothing
                     else
                         let args1 = List.take nps args
                             args2 = List.drop nps args
                         in 
                             case matchingList ps args1 Dict.empty of
                                 Nothing -> dispatchAlts alts1 args
                                 Just s ->
                                     let
                                         ne = applyArgs (AST.applySubst s e) args2
                                     in
                                         Just (ne, info)
                                        
    

-- perform a single step reduction    
redex : Functions -> Expr -> Maybe (Expr,String)
redex functions expr =
    case expr of
        Var x ->
            redex functions (App (Var x) []) 
        
        App e1 es ->
            case unwindArgs e1 es of
                (Lam xs e0, args) ->
                    let
                        alt = (List.map VarP xs, e0, "beta-reduction")
                    in 
                        dispatchAlts [alt] args
                            
                (Var fun, args) ->
                    case Dict.get fun functions of
                        Just semantics ->
                            semantics args
                        Nothing ->
                            Just (Fail "undefined name", fun)
                _ ->
                    Just (Fail "invalid function", "error")
                    
        Cons e1 (ListLit l) ->
            Just (ListLit (e1::l), "constructor")
                
        InfixOp op e1 e2 ->
            redex functions (App (Var op) [e1, e2])

        IfThenElse e1 e2 e3 ->
            case e1 of
                Boolean True -> Just (e2, "if-True")
                Boolean False -> Just (e3, "if-False")
                _ -> if isWeakNormalForm e1
                     then Just (Fail "type error: if requires a boolean", "if")
                     else Nothing

        EnumFrom (Number a) ->
            Just ( Cons (Number a) (EnumFrom (Number (a+1)))
                 , "enumeration" )

        EnumFromThen (Number a1) (Number a2) ->
            let
                a3 = 2*a2 - a1
            in 
                Just ( Cons (Number a1) (EnumFromThen (Number a2) (Number a3))
                     , "enumeration" )

        EnumFromTo (Number a) (Number b) ->
            Just ( ListLit (List.map Number <| ranged a b 1)
                 , "enumeration" )

        EnumFromThenTo (Number a1) (Number a2) (Number b) ->
            Just (ListLit (List.map Number <| ranged a1 b (a2-a1))
                 , "enumeration")
            
                         
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
        (TupleP ps, TupleLit es) ->
            forceEvalList ps es
        (ListP ps, ListLit es) ->
            forceEvalList ps es
        (ListP [], Cons _ _) -> False
        (ListP (p1::ps), Cons e1 e2) ->
            forceEval p1 e1 || forceEval (ListP ps) e2
        (ConsP p1 p2, Cons e1 e2) ->
            forceEval p1 e1 || forceEval p2 e2
        (ConsP p1 p2, ListLit [])  -> False
        (ConsP p1 p2, ListLit (e1::rest))  ->
            forceEval p1 e1 || forceEval p2 (ListLit rest)
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
          (ListP []) ->
              case e of
                  ListLit [] -> Just s
                  _ -> Nothing
                       
          (ListP (p1::ps)) ->
              case e of
                  ListLit (e1::es) -> matching p1 e1 s
                                   |> Maybe.andThen (matching (ListP ps) (ListLit es))
                  Cons e1 e2 -> matching p1 e1 s
                                |> Maybe.andThen (matching (ListP ps) e2)
                  _ ->
                      Nothing

          (TupleP ps) ->
              case e of
                  TupleLit es -> matchingList ps es s
                  _ -> Nothing
              
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
          ([], []) -> Just s
          _ -> Nothing


       
-- * perform a single reduction under a context
redexCtx : Functions -> Expr -> Context -> Maybe (Expr,String)
redexCtx functions expr ctx
    = ctx.getOption expr
          |> Maybe.andThen
             (\subexpr ->
                  redex functions subexpr
             |> Maybe.andThen
                  (\result ->
                       case result of
                           (Fail err, info) -> Just (Fail err, info)
                           (new,info) -> Just (ctx.set new expr, info)))
                       -- (\(new, info) -> Just (ctx.set new expr, info)))


                            
-- locate the outermost-leftmost redex
outermostRedex : Functions -> Expr -> Maybe Context
outermostRedex functions expr =
    case redex functions expr of
        Just _ ->
            Just Context.hole
        Nothing ->
            outermostRedexAux functions expr 

outermostRedexAux : Functions -> Expr -> Maybe Context
outermostRedexAux functions expr 
    = case expr of
          (Cons e0 e1) ->
              case outermostRedex functions e0 of
                  Just ctx ->
                      Just (Monocle.compose Context.cons0 ctx)
                  Nothing ->
                      case outermostRedex functions e1 of
                          Just ctx ->
                              Just (Monocle.compose Context.cons1 ctx)
                          Nothing ->
                              Nothing
                                  
          (InfixOp op e0 e1) ->
              case outermostRedex functions e0 of
                  Just ctx ->
                      Just (Monocle.compose Context.infixOp0 ctx)
                  Nothing ->
                      case outermostRedex functions e1 of
                          Just ctx ->
                              Just (Monocle.compose Context.infixOp1 ctx)

                          Nothing -> Nothing

          (App e0 args) ->
              case outermostRedex functions e0 of
                  Just ctx ->
                      Just (Monocle.compose Context.app0 ctx)
                  Nothing ->
                      outermostRedexArgs functions Context.appArg args 0

          (TupleLit items) ->
              outermostRedexArgs functions Context.tupleItem items 0

          (ListLit items) ->
              outermostRedexArgs functions Context.listItem items 0

          (IfThenElse e0 e1 e2) ->
              case outermostRedex functions e0 of
                  Just ctx ->
                      Just (Monocle.compose Context.if0 ctx)
                  Nothing ->
                      Nothing
                          
          _ ->
              Nothing



-- first try to reduce arguments that aren't in WHNF
outermostRedexArgs :
    Functions -> (Int -> Context) -> List Expr  -> Int -> Maybe Context
outermostRedexArgs = outermostRedexArgs1
                 
{-
                 outermostRedexArgs functions proj args i
    = case outermostRedexArgs1 functions proj args i of
          Just ctx ->
              Just ctx
          Nothing ->
              outermostRedexArgs2 functions proj args i
-}

 -- * try to reduce arguments that aren't Whnfs                 
outermostRedexArgs1 :
    Functions -> (Int -> Context) -> List Expr  -> Int -> Maybe Context
outermostRedexArgs1 functions proj args i =
    case args of
        (arg::rest) ->
            if isWeakNormalForm arg then
                outermostRedexArgs1 functions proj rest (i+1)
            else
                case outermostRedex functions arg of
                    Just ctx ->
                        Just (Monocle.compose (proj i) ctx)
                    Nothing ->
                        outermostRedexArgs1 functions proj rest (i+1)
        [] ->
            Nothing

-- * try to reduce any argument by left to right order
outermostRedexArgs2 :
    Functions -> (Int -> Context) -> List Expr  -> Int -> Maybe Context
outermostRedexArgs2 functions proj args i =
    case args of
        (arg::rest) ->
            case outermostRedex functions arg of
                Just ctx ->
                    Just (Monocle.compose (proj i) ctx)
                Nothing ->
                    outermostRedexArgs2 functions proj rest (i+1)
        [] ->
            Nothing
    


-- check if an expression is a weak normal form

isWeakNormalForm : Expr -> Bool
isWeakNormalForm expr =
    case expr of
        App _ _ ->
            False
        Lam _ _ ->
            True
        Var _ ->
            False
        Number _ ->
            True
        Boolean _ ->
            True
        Cons _ _ ->
            True
        ListLit _ ->
            True
        TupleLit _ ->
            True
        _ ->
            False


isNormalForm : Expr -> Bool
isNormalForm expr =
    case expr of
        App _ _ ->
            False
        Lam _ _ ->
            True
        Var _ ->
            False
        Number _ ->
            True
        Boolean _ ->
            True
        Cons e1 e2 ->
            isNormalForm e1 && isNormalForm e2
        ListLit items ->
            List.all isNormalForm items
        TupleLit items ->
            List.all isNormalForm items
        _ ->
            False


-- like List.range but with a variable step (delta)
ranged : Int -> Int -> Int -> List Int
ranged a b delta =
    if delta>0 then
        rangedUp a b delta
    else if delta<0 then
             rangedDown a b delta
         else []
             
rangedUp a b delta = if a <= b then a :: rangedUp (a+delta) b delta else []

rangedDown a b delta = if a >= b then a :: rangedDown (a+delta) b delta else []
