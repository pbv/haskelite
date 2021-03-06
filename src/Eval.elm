{-
  Single-step evaluation of Haskelite programs
  Pedro Vasconcelos, 2021
-} 
module Eval exposing (..)

import AST exposing (Expr(..), Pattern(..), Decl(..), Info(..), Name, Subst)
import Pretty
import Dict exposing (Dict)
import Context exposing (Context)
import Monocle.Optional as Monocle


-- * semantics
-- global function bindings
type alias Binds
    = Dict AST.Name (List Alt)

-- alternatives equations for a single function
type alias Alt 
    = (List Pattern, Expr)

-- Semantics of primitive operations: a function from global
-- definitions and the stack of arguments after unwinding to an
-- optional expression; a result of Nothing means that the function
-- does not reduce; the string is an human readable explanation for
-- the reduction (either the equation employed or some primitive
-- operation).
-- This needs globals because primitives may force the evaluation
-- of arguments.
type alias Prim
    = Binds -> List Expr -> Maybe (Expr, Info)


-- transform a list of declarations into a dictionary for functions
collectBindings : List Decl -> Binds -> Binds
collectBindings decls accum
    = case decls of
          [] ->
              accum
          (Equation fun ps e :: rest) ->
              let
                  -- info = Pretty.prettyDecl (Equation fun ps e)
                  (alts1,rest1) = collectAlts fun rest
                  accum1 = Dict.insert fun ((ps,e)::alts1) accum
              in collectBindings rest1 accum1
          (_ :: rest) ->
              collectBindings rest accum
                  
                  

-- collect all contiguous equations for a given name
collectAlts : Name -> List Decl -> (List Alt, List Decl)
collectAlts fun decls
    = case decls of
          [] -> ([], [])
          (TypeSig _ _ :: rest) ->
              ([], rest)
          (Comment _ :: rest) ->
              collectAlts fun rest
          (Equation f ps e :: rest) ->
              if f==fun then
                  let
                      (alts, rest1) = collectAlts fun rest
                  in ((ps,e)::alts, rest1)
              else
                  ([], decls)
    
    
-- built-in operations
primitives : Dict AST.Name Prim
primitives
    = Dict.fromList
      [ (":", consPrim)
      , ("+", arithOp "+" (+))
      , ("-", arithOp "-" (-))
      , ("*", arithOp "*" (*))
      , ("div", arithOp "div" (//))
      , ("mod", arithOp "mod" (\x y -> modBy y x))
      , ("/=", compareOp "/=" (/=))
      , ("==", compareOp "==" (==))
      , (">=",  compareOp ">=" (>=))
      , ("<=", compareOp "<=" (<=))
      , (">", compareOp ">" (>))
      , ("<", compareOp "<" (<))
      , ("enumFrom", enumFrom)
      , ("enumFromThen", enumFromThen)
      , ("enumFromTo", enumFromTo)
      , ("enumFromThenTo", enumFromThenTo)
      , ("negate", arithNeg)
      ]


mkCons : Expr -> Expr -> Expr
mkCons e1 e2 = App (Var ":") [e1,e2]

    
consPrim : Binds -> List Expr -> Maybe (Expr, Info)
consPrim global args
    = case args of
          [e1, ListLit l] ->
              Just (Eval (ListLit (e1::l)), Prim "constructor")
          [e1, TupleLit _] ->
              Just (Fail "type error", Prim "constructor")
          [e1, Boolean _] ->
            Just (Fail "type error", Prim "constructor")

          [e1, Number _] ->
            Just (Fail "type error", Prim "construtor")

          [e1, Lam _ _] ->
            Just (Fail "type error", Prim "construtor")

          _ -> Nothing
                  
                    

arithNeg : Binds -> List Expr -> Maybe (Expr, Info)
arithNeg globals args
    = case args of
          [Number x] ->
              Just (Eval (Number (-x)), Prim "negate")
          [arg1] ->
              if isWeakNormalForm arg1 then
                  Just (Fail "type error: operator requires numbers"
                       , Prim "negate")
              else
                  redex globals arg1
                      |> Maybe.andThen (\(narg1,info) ->
                                            Just (App (Var "negate") [narg1]
                                                 , info))
          _ -> if List.length args > 2 then
                   Just (Fail "type error: wrong number of arguments"
                        ,Prim "negate")
               else 
                   Nothing
                  
    
arithOp : Name -> (Int -> Int -> Int)
        -> Binds -> List Expr -> Maybe (Expr, Info)
arithOp op func globals args
    = case args of
          [Number x, Number y] ->
              Just (Eval (Number (func x y)), Prim op)
          [arg1, arg2] ->
              if isWeakNormalForm arg1 && isWeakNormalForm arg2 then
                  Just (Fail "type error: operator requires numbers"
                       , Prim op)
              else
                  case redex globals arg1 of
                      Just (narg1, info) ->
                          Just (InfixOp op narg1 arg2, info)
                      Nothing ->
                          case redex globals arg2 of
                              Just (narg2, info) ->
                                  Just (InfixOp op arg1 narg2, info)
                              Nothing ->
                                  Nothing
          _ -> if List.length args > 2 then
                   Just (Fail "type error: wrong number of arguments", Prim op)
               else 
                   Nothing

-- simple comparisons for numbers only
compareOp : Name -> (Int -> Int -> Bool)
          -> Binds -> List Expr -> Maybe (Expr,Info)
compareOp op func globals args
    = case args of
          [Number x, Number y] ->
              Just (Eval (Boolean (func x y)), Prim op)
          [arg1, arg2] ->
              if isWeakNormalForm arg1 && isWeakNormalForm arg2 then
                  Just (Fail "type error: operator requires numbers", Prim op)
              else
                  case redex globals arg1 of
                      Just (narg1, info) ->
                          Just (InfixOp op narg1 arg2, info)
                      Nothing ->
                          case redex globals arg2 of
                              Just (narg2, info) ->
                                  Just (InfixOp op arg1 narg2, info)
                              Nothing ->
                                  Nothing
          _ -> if List.length args > 2 then
                   Just (Fail "type error: wrong number of arguments", Prim op)
               else
                   Nothing

enumFrom : Binds -> List Expr -> Maybe (Expr, Info)
enumFrom globals args
    = case args of
          [Number a] ->
              Just (Eval (mkCons (Number a) (App (Var "enumFrom") [Number (a+1)]))
                   , Prim "enumeration" )
          [e1] ->
              redex globals e1
                  |> Maybe.andThen (\(ne1,info) ->
                                        Just (App (Var "enumFrom") [ne1]
                                             , info))
          _ -> Nothing
               
enumFromThen : Binds -> List Expr -> Maybe (Expr, Info)
enumFromThen globals args
    = case args of
          [Number a1, Number a2] ->
              let
                  a3 = 2*a2 - a1
              in
                  Just (Eval (mkCons (Number a1)
                             (App (Var "enumFromThen") [Number a2, Number a3]))
                       , Prim "enumeration" )
          [e1, e2] ->
              case redex globals e1 of
                  Just (ne1,info) -> Just (App (Var "enumFromThen") [ne1,e2]
                                          , info)
                  Nothing ->
                      redex globals e2
                          |> Maybe.andThen (\(ne2,info) ->
                                                Just (App (Var "enumFromThen") [e1,ne2]
                                                     , info))
          _ -> Nothing
           
enumFromTo : Binds -> List Expr -> Maybe (Expr, Info)
enumFromTo globals args
    = case args of
          [Number a, Number b] ->
              Just (Eval (ListLit <| List.map Number <| ranged a b 1)
                   , Prim "enumeration" )
          [e1, e2] ->
              case redex globals e1 of
                  Just (ne1,info) -> Just (App (Var "enumFromTo") [ne1,e2]
                                          , info)
                  Nothing ->
                      redex globals e2
                          |> Maybe.andThen (\(ne2,info) ->
                                                Just (App (Var "enumFromTo") [e1,ne2]
                                                     , info))
          _ -> Nothing
                       

enumFromThenTo : Binds -> List Expr -> Maybe (Expr, Info)
enumFromThenTo globals args
    = case args of
          [Number a1, Number a2, Number b] ->
              Just (Eval (ListLit <| List.map Number <| ranged a1 b (a2-a1))
                   , Prim "enumeration" )
          [e1, e2, e3] ->
              case redex globals e1 of
                  Just (ne1,info) -> Just (App (Var "enumFromThenTo") [ne1,e2,e3]
                                          , info)
                  Nothing ->
                      case redex globals e2 of
                          Just (ne2,info) ->
                              Just (App (Var "enumFromThenTo") [e1,ne2,e3], info)
                          Nothing ->
                              redex globals e3
                                  |> Maybe.andThen (\(ne3,info) ->
                                                        Just (App (Var "enumFromThenTo") [e1,e2,ne3], info))
          _ -> Nothing
                       

               
-- apply a function specifified by a list of alterantives
-- to a list of arguments
-- result is Nothing if the expression can't be reduced yet
dispatchAlts : Binds -> Name -> List Alt -> List Expr -> Maybe (Expr, Info)
dispatchAlts globals fun alts args
    = case alts of
          [] -> Just (Fail "pattern match failure", Prim "error")
          ((ps,e)::alts1) ->
              let nps = List.length ps
                  nargs = List.length args
              in if nargs < nps
                 then Nothing
                 else
                     let args1 = List.take nps args
                         args2 = List.drop nps args
                     in
                         case patternEvalList globals ps args1 [] of
                             Just (nargs1,info)  ->
                                 let
                                     ne = applyArgs (makeApp fun nargs1) args2
                                 in
                                     Just (ne, info)
                             Nothing ->
                                 case matchingList ps args1 Dict.empty of
                                     Nothing ->
                                         dispatchAlts globals fun alts1 args
                                     Just s ->
                                         let
                                             ne = applyArgs (Eval (AST.applySubst s e)) args2
                                             info = Rewrite (Equation fun ps e)
                                         in
                                             Just (ne, info)


dispatchBeta : List Name -> Expr -> List Expr -> Maybe (Expr, Info)
dispatchBeta vars body args =
    let
        nvars = List.length vars
        nargs = List.length args
    in
        if nargs < nvars then
            Nothing
        else
            let args1 = List.take nvars args
                args2 = List.drop nvars args
                s = Dict.fromList (List.map2 Tuple.pair vars args1)
                ne = applyArgs (Eval (AST.applySubst s body)) args2
                info = Prim "beta reduction"
            in
                Just (ne, info)
                
makeApp : Name -> List Expr -> Expr
makeApp fun args
    = case args of
          [arg1, arg2] -> if Pretty.isOperator fun then
                              InfixOp fun arg1 arg2
                          else
                              App (Var fun) args
          _ -> App (Var fun) args

-- perform the next single step reduction
-- to evaluate an expression to weak head normal form
redex : Binds -> Expr -> Maybe (Expr,Info)
redex globals expr =
    case expr of
        Var x ->
            redex globals (App (Var x) []) 
        
        App e1 es ->
            case unwindArgs e1 es of
                (Lam xs e0, args) ->
                    dispatchBeta xs e0 args
                            
                (Var fun, args) ->
                    case Dict.get fun globals of
                        Just alts ->
                            dispatchAlts globals fun alts args
                        Nothing ->
                            case Dict.get fun primitives of
                                Just prim ->
                                    prim globals args
                                Nothing ->
                                    Just (Fail "undefined name"
                                         , Prim "application")
                _ ->
                    Just (Fail "invalid function", Prim "application")
                    
        InfixOp op e1 e2 ->
            redex globals (App (Var op) [e1, e2])

        IfThenElse e1 e2 e3 ->
            case e1 of
                Boolean True -> Just (Eval e2, Prim "if-True")
                Boolean False -> Just (Eval e3, Prim "if-False")
                _ -> if isWeakNormalForm e1
                     then
                         Just (Fail "type error", Prim "if")
                     else
                         redex globals e1
                             |> Maybe.andThen (\(ne1,info) ->
                                                   Just (IfThenElse ne1 e2 e3,info))

        Eval e1 ->
            redex globals e1
                                
        _ ->
            Nothing

             

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

              

-- perform pattern matching
matching : Pattern -> Expr -> Subst -> Maybe Subst
matching p e s
    = case p of
          (VarP x) ->
              Just (Dict.insert x e s)

          (BangP x) ->
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
                  App (Var ":") [e1, e2] -> matching p1 e1 s
                                |> Maybe.andThen (matching (ListP ps) e2)
                  _ ->
                      Nothing

          (TupleP ps) ->
              case e of
                  TupleLit es -> matchingList ps es s
                  _ -> Nothing
              
          (ConsP p1 p2) ->
              case e of
                  (App (Var ":") [e1, e2]) -> matching p1 e1 s
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




                   


               
-- 
-- check if a pattern performs evaluation of subexpression
-- returns Just (newexpr, info) if it forces the evaluation
-- or Nothing otherwise
--
patternEval : Binds -> Pattern -> Expr -> Maybe (Expr, Info)
patternEval globals p e =
    case (p, e) of
        (VarP _, _) -> Nothing
        (NumberP _, Number _) -> Nothing
        (BooleanP _, Boolean _) -> Nothing
        (TupleP ps, TupleLit es) ->
            patternEvalList globals ps es []
                |> Maybe.andThen (\(nes, info) -> Just (TupleLit nes, info))
        (ListP ps, ListLit es) ->
            patternEvalList globals ps es []
                |> Maybe.andThen (\(nes,info) -> Just (ListLit nes, info))
        (ListP [], App (Var ":") _) -> Nothing
        (ListP (p1::ps), App (Var ":") [e1, e2]) ->
            case patternEval globals p1 e1 of
                Just (ne1,info) ->
                    Just (App (Var ":") [ne1, e2], info)
                Nothing ->
                    patternEval globals (ListP ps) e2
                        |> Maybe.andThen (\(ne2,info) ->
                                              Just (App (Var ":") [e1, ne2],info))
        (ConsP p1 p2, ListLit []) -> Nothing
        (ConsP p1 p2, ListLit (e1::rest)) ->
            case patternEval globals p1 e1 of
                Just (ne1,info) ->
                    Just (ListLit (ne1::rest),info)
                Nothing ->
                    patternEval globals p2 (ListLit rest)
                        |> Maybe.andThen (\(nrest_,info) ->
                                              case nrest_ of
                                                  ListLit nrest ->
                                                      Just (ListLit (e1::nrest), info)
                                                  _ -> Nothing)
                                         
        (_, _) ->
            redex globals e

                  
patternEvalList :
    Binds -> List Pattern -> List Expr -> List Expr -> Maybe (List Expr,Info)
patternEvalList globals patts exprs accum =
    case (patts, exprs) of
        (p1::ps, e1::es) ->
            case patternEval globals p1 e1 of
                Just (ne1,info) ->
                    Just (List.reverse accum ++ (ne1::es), info)
                Nothing ->
                    patternEvalList globals ps es (e1::accum)
        (_, _) -> Nothing


    

               
-- * perform a single reduction under a context
redexCtx : Binds -> Expr -> Context -> Maybe (Expr,Info)
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


                            
-- locate the next outermost-leftmost redex
-- to evaluate an expression to head normal form;
-- does not evaluate under lambdas or if branches
outermostRedex : Binds -> Expr -> Maybe Context
outermostRedex globals expr =
    case redex globals expr of
        Just _ ->
            Just Context.hole
        Nothing ->
            outermostRedexAux globals expr 

outermostRedexAux : Binds -> Expr -> Maybe Context
outermostRedexAux globals expr 
    = case expr of
          (App (Var ":") [e0, e1]) ->
              case outermostRedex globals e0 of
                  Just ctx ->
                      Just (Monocle.compose Context.cons0 ctx)
                  Nothing ->
                      case outermostRedex globals e1 of
                          Just ctx ->
                              Just (Monocle.compose Context.cons1 ctx)
                          Nothing ->
                              Nothing
                                     
          (TupleLit items) ->
              outermostRedexArgs globals Context.tupleItem items 0

          (ListLit items) ->
              outermostRedexArgs globals Context.listItem items 0

          (IfThenElse e0 e1 e2) ->
              case outermostRedex globals e0 of
                  Just ctx ->
                      Just (Monocle.compose Context.if0 ctx)
                  Nothing ->
                      Nothing 
          _ ->
              Nothing



-- * try to reduce some argument by left to right order
             
outermostRedexArgs :
    Binds -> (Int -> Context) -> List Expr  -> Int -> Maybe Context
outermostRedexArgs functions proj args i =
    case args of
        (arg::rest) ->
            case outermostRedex functions arg of
                Just ctx ->
                    Just (Monocle.compose (proj i) ctx)
                Nothing ->
                    outermostRedexArgs functions proj rest (i+1)
        [] ->
            Nothing
    


reduceNext : Binds -> Expr -> Maybe (Expr,Info)
reduceNext globals expr
    = let expr1 = AST.uneval expr
      in outermostRedex globals expr1
              |> Maybe.andThen (\ctx -> redexCtx globals expr1 ctx)

           
isNormalForm : Binds -> Expr -> Bool
isNormalForm functions expr
    = case reduceNext functions expr of
          Just _ -> False
          Nothing -> True

reduceFull : Binds -> Expr -> Maybe (Expr, Info)
reduceFull globals expr
    = reduceNext globals expr
      |> Maybe.andThen (\(expr1,_) -> Just (reduceFullAux globals expr1, Prim "..."))
                     
reduceFullAux : Binds -> Expr -> Expr
reduceFullAux globals expr
    = case reduceNext globals expr of
          Just (expr1,_) ->
              reduceFullAux globals expr1
          Nothing ->
              expr

                     
-- check if an expression is a weak normal form

isWeakNormalForm : Expr -> Bool
isWeakNormalForm expr =
    case expr of
        App (Var ":") _ ->
            True
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
        ListLit _ ->
            True
        TupleLit _ ->
            True
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
             
rangedUp a b delta
    = if a <= b then a :: rangedUp (a+delta) b delta else []

rangedDown a b delta
    = if a >= b then a :: rangedDown (a+delta) b delta else []
