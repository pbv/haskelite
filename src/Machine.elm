{- 
  A Lazy abstract machine based on the PMC calculus

  Pedro Vasconcelos, 2023
-}
module Machine exposing (..)

import AST exposing (Expr(..),
                     Matching(..),
                     Pattern(..),
                     Bind,
                     Decl(..),
                     Info,
                     Name,
                     Tag,
                     Subst)
import Heap exposing (Heap)
import Context exposing (ExprCtx)
import Dict exposing (Dict)
import Set exposing (Set)
import Monocle.Optional as Monocle


type alias Conf
    = (Heap, Control, Stack)

type Control
    = E Expr
    | M Matching ArgStack

type alias ArgStack
    = List Expr
      
type alias Stack
    = List Cont

type Cont
    = PushArg Expr
    | Update Name
    | MatchEnd Skip
    | PushAlt ArgStack Matching
    | PushPat ArgStack Pattern Matching
      -- continuations for primitive operations
    | ContBinary Name Expr
    | RetBinary Name Expr
    | RetUnary Name
      -- for full normal form evaluation
    | DeepEval
    | Continue Expr ExprCtx

type Skip
    = Skipped | NotSkipped
      

      
isWhnf : Expr -> Bool
isWhnf expr =
    case expr of
        Lam 0 _ m ->      --- AST.matchingArity m == 0
            False
        Lam _ _ m ->
            True          ---  AST.matchingArity m > 0
        Number _ ->
            True
        Char _ ->
            True
        Cons _ _ ->
            True
        _ ->
            False


isVar : Expr -> Bool
isVar expr =
    case expr of
        Var _ ->
            True
        _ ->
            False
              

getHeap : Conf -> Heap
getHeap (heap,_,_) = heap

getControl : Conf -> Control
getControl (_, control, _) = control

getStack : Conf -> Stack
getStack (_, _, stack) = stack           
                      
                         
                         
--                
-- a small-step transition of the machine
-- first argument is the set of functions to skip over
transition : Set Name -> Conf -> Maybe Conf
transition skipped conf
    = case conf of
          -- short circuit errors
          (heap, E (Error msg), _::_) ->
              Just (heap, E (Error msg), [])

          -- applications and constructors
          (heap,  E (App e1 e2), stack) ->
              Just (heap, E e1, PushArg e2::stack)

          (heap, E (Cons c args), PushArg arg::stack) ->
              Just (heap, E (Cons c (args ++ [arg])), stack)

          -- saturated lambda: go into matching evaluation
          (heap, E (Lam 0 optname m), stack) ->
              case optname of
                  Just fun ->
                      if Set.member fun skipped then
                          Just (heap, M m [], MatchEnd Skipped::DeepEval::stack)
                      else
                          Just (heap, M m [], MatchEnd NotSkipped::stack)
                  Nothing ->
                      Just (heap, M m [], MatchEnd NotSkipped::stack)

          -- apply argument to non-saturated lambda
          (heap, E (Lam _ optname m), PushArg e1::rest) ->
              -- put into A-normal form if necessary:
              -- check if we neeed to update the result of evaluating `e1'
              if isVar e1 || isWhnf e1 then
                  -- no indirection needed
                  Just (heap, E (AST.lambda optname (Arg e1 m)), rest)
              else
                  -- create a new indirection to the expression
                  let
                      (loc, heap1) = Heap.newIndirection heap e1
                  in
                      Just (heap1, E (AST.lambda optname (Arg (Var loc) m)), rest)  

          -- local bindings
          (heap, E (Let binds e1), stack) ->
              let
                  (s, heap1) = Heap.newBindings heap binds 
              in
                  Just (heap1, E (AST.applySubst s e1), stack)

          -- case expressions
          (heap, E (Case e1 alts), stack) ->
              Just (heap, M (AST.translateCase e1 alts) [],
                        MatchEnd NotSkipped::stack)                              
                      
          -- if-then-else
          (heap, E (IfThenElse e1 e2 e3), stack) ->
              Just (heap, M (AST.translateIfThenElse e1 e2 e3) [],
                        MatchEnd NotSkipped::stack)

          (heap, E (Var y), stack) ->
              case Heap.get y heap of
                  Just expr ->
                      if isWhnf expr then
                          Just (heap, E expr, stack)
                      else
                          -- blackhole the location before evaluating
                          -- let
                          --    heap1 = Dict.remove y heap
                          -- in
                          -- NB: THIS IS COMMENTED OUT SO WE CAN
                          -- PRETTY-PRINT INDIRECTIONS!
                          Just (heap, E expr, Update y::stack)
                  _ ->
                      Nothing

          -- primitive operations
          (heap, E (BinaryOp op e1 e2), stack) ->
              Just (heap, E e1, (ContBinary op e2)::stack)
                  
          (heap, E (UnaryOp op e1), stack) ->
              Just (heap, E e1, (RetUnary op)::stack)

          -- the remaining expression evaluation rules
          -- should apply only to whnfs
          (heap, E w1, (ContBinary op e2)::stack) ->
              Just (heap, E e2, (RetBinary op w1)::stack)
                        
          (heap, E w2, (RetBinary op w1)::stack) ->
              Just (heap, E (applyPrimitive2 op w1 w2), stack)

          (heap, E w, (RetUnary op)::stack) ->
              Just (heap, E (applyPrimitive1 op w), stack)
                  
          -- update variable
          (heap, E w, Update y::stack) ->
              if isWhnf w then
                  let
                      heap1 = Heap.update y w heap
                  in
                      Just (heap1, E w, stack)
              else
                  Nothing

          -- ignore an argument
          (heap, M (Match DefaultP m1) (e1::args), stack) ->
              Just (heap, M m1 args, stack)
                      
          -- bind a variable
          (heap, M (Match (VarP x) m1) (e1::args), stack) ->
              let
                  m2 = AST.applyMatchSubst (Dict.singleton x e1) m1
              in
                  Just (heap, M m2 args, stack)
                      
          -- match a constructor or a strict (bang) pattern 
          (heap, M (Match p1 m1) (e1::args), stack) ->
              Just (heap, E e1, (PushPat args p1 m1)::stack)
                      
          -- decompose a constructor 
          (heap, E (Cons c0 es), (PushPat args (ConsP c1 ps) m1)::stack) ->
              if c0 == c1 && List.length es == List.length ps then
                  let (heap1, es1) = normalizeConsArgs heap es
                  in 
                  Just (heap1, M (matchCons es1 ps m1) args, stack) 
              else
                  Just (heap, M Fail [], stack)

          (heap, E w, (PushPat args (BangP x) m1)::stack) ->
              let
                  m2 = AST.applyMatchSubst (Dict.singleton x w) m1
              in
                  Just (heap, M m2 args, stack)                          

          -- match numbers
          (heap, E (Number n), (PushPat args (NumberP k) m1)::stack) ->
              if n == k then
                  Just (heap, M m1 args, stack)
              else
                  Just (heap, M Fail [], stack)

          -- match characters
          (heap, E (Char c), (PushPat args (CharP k) m1)::stack) ->
              if c == k then
                  Just (heap, M m1 args, stack)
              else
                  Just (heap, M Fail [], stack)
                                
          -- successful match: return an expression
          (heap, M (Return expr info) args, MatchEnd _::stack) ->
              Just (heap, E (applyArgs expr args), stack)

          (heap, M (Return expr info) args, (PushAlt _ _)::stack) ->
              Just (heap, M (Return expr info) args, stack)

          -- failing matches
          (heap, M Fail _, (PushAlt args m)::stack) ->
              Just (heap, M m args, stack)

          (heap, M Fail _, MatchEnd _::stack) ->
              Just (heap, E (Error (AST.stringLit "non-exaustive patterns")), [])
                  
          -- deal with alternatives
          (heap, M (Alt m1 m2) args, stack) ->
              Just (heap, M m1 args, PushAlt args m2::stack)

          -- deal with arguments
          (heap, M (Arg e m1) args, stack) ->
              Just (heap, M m1 (e::args), stack)

                  
          -- deep evaluation
          -- NB: this does not preserve sharing
          (heap, E w, DeepEval::stack) ->
              Just (deepEval w heap stack)
                  
          -- NB: this does not preserve sharing
          (heap, E w, (Continue expr ctx)::stack) ->
              Just (deepEval (ctx.set w expr) heap stack)
                    
          _ ->
              Nothing

matchCons : List Expr -> List Pattern -> Matching -> Matching
matchCons es ps m
    = case (es, ps) of
          (e::es1, p::ps1) ->
              Arg e (Match p (matchCons es1 ps1 m))
          _ ->
              m

applyArgs : Expr -> ArgStack -> Expr
applyArgs expr args =
    case args of
        [] ->
            expr
        (arg1::rest) ->
            applyArgs (App expr arg1) rest
              

--                
-- apply a binary primitive
-- the arguments should be in whnf
--
applyPrimitive2 : Name -> Expr -> Expr -> Expr
applyPrimitive2 op e1 e2
    = case (op, e1, e2) of
          ("+", Number v1, Number v2) ->
              Number (v1 + v2)
          ("-", Number v1, Number v2) ->
              Number (v1 - v2)
          ("*", Number v1, Number v2) ->
              Number (v1 * v2)
          ("div", Number v1, Number v2) ->
              if v2 /= 0 then
                   Number (v1 // v2)
               else
                   Error (AST.stringLit "division by zero")
          ("mod", Number v1, Number v2) ->
              if v2 /= 0 then
                   Number (modBy v2 v1)
               else
                   Error (AST.stringLit "division by zero")
          ("compare", _, _) ->
              structuralCmp e1 e2
          ("==", _, _) ->
              structuralEq e1 e2
          ("/=", _, _) ->
              App (Var "not") (structuralEq e1 e2)
          ("<=", _, _) ->
              orderingCase (structuralCmp e1 e2)
                  AST.trueCons AST.trueCons AST.falseCons
          ("<", _, _) ->
              orderingCase (structuralCmp e1 e2)
                  AST.trueCons AST.falseCons AST.falseCons              
          (">=", _, _) ->
              orderingCase (structuralCmp e1 e2)
                  AST.falseCons AST.trueCons AST.trueCons
          (">", _, _) ->
              orderingCase (structuralCmp e1 e2)
                  AST.falseCons AST.falseCons AST.trueCons              
          _ ->
              Error (AST.stringLit "invalid primitive arguments")


-- apply a unary primitive
-- the argument should be in whnf
applyPrimitive1 : Name -> Expr -> Expr
applyPrimitive1 op e
    = case (op, e) of
          ("negate", Number v) ->
              Number (-v)
          ("toLower", Char c) ->
              Char (Char.toLower c)
          ("toUpper", Char c) ->
              Char (Char.toUpper c)
          ("isUpper", Char c) ->
              compareOp (Char.isUpper c)
          ("isLower", Char c) ->
              compareOp (Char.isLower c)
          ("isAlpha", Char c) ->
              compareOp (Char.isAlpha c)
          ("isAlphaNum", Char c) ->
              compareOp (Char.isAlphaNum c)
          ("isDigit", Char c) ->
              compareOp (Char.isDigit c)
          ("ord", Char c) ->
              Number (Char.toCode c)
          ("chr", Number n) ->
              Char (Char.fromCode n)
          _  ->
              Error (AST.stringLit "invalid primitive arguments")


--                
-- normalize constructor arguments to A-normal form
-- i.e., introduce indirections for arguments that are not whnfs or variables
--
normalizeConsArgs : Heap -> List Expr -> (Heap, List Expr)
normalizeConsArgs heap args
    = case args of
          [] ->
              (heap, [])
          (arg1::rest) ->
              if isVar arg1 || isWhnf arg1 then
                  let (heap1, rest1) = normalizeConsArgs heap rest
                  in (heap1, arg1::rest1)
              else
                  let (loc, heap1) = Heap.newIndirection heap arg1
                      (heap2, rest1) = normalizeConsArgs heap1 rest
                  in (heap2, Var loc::rest1)

-- polymorphic structural equality (==)
-- assumes arguments are already in whnf
-- but may produce a result that is not in whnf
structuralEq :  Expr -> Expr -> Expr
structuralEq e1 e2
    = case (e1, e2) of
          (Number v1, Number v2) ->
              compareOp (v1 == v2)
          (Char c1, Char c2) ->
              compareOp (c1 == c2)
          (Cons c1 args1, Cons c2 args2) ->
              if c1 == c2 then
                  structuralEqList (List.map2 Tuple.pair args1 args2)
              else
                  AST.falseCons
          (Lam _ _ _, Lam _ _ _) ->
              Error (AST.stringLit "can't compare functions")
          _ ->
              Error (AST.stringLit "invalid arguments to equality")

structuralEqList : List (Expr,Expr) -> Expr
structuralEqList args
    = case args of
          [] ->
              AST.trueCons
          [(e1,e2)] ->
              BinaryOp "==" e1 e2
          ((e1,e2)::rest) ->
              App (App (Var "&&") (BinaryOp "==" e1 e2)) (structuralEqList rest)


  
structuralCmp : Expr -> Expr -> Expr
structuralCmp e1 e2
    = case (e1, e2) of
          (Number v1, Number v2) ->
              orderingOp (compare v1 v2)
          (Char c1, Char c2) ->
              orderingOp (compare c1 c2)
          (Cons c1 args1, Cons c2 args2) ->
              case compareCons c1 c2 of
                  LT ->
                      Cons "LT" []
                  EQ ->
                      structuralCmpList (List.map2 Tuple.pair args1 args2)
                  GT ->
                      Cons "GT" []
          (Lam _ _ _, Lam _ _ _) ->
              Error (AST.stringLit "can't compare functions")
          _ ->
              Error (AST.stringLit "invalid arguments to comparison")

structuralCmpList : List (Expr,Expr) -> Expr
structuralCmpList args
    = case args of
          [] ->
              Cons "EQ" []
          [(e1,e2)] ->
              BinaryOp "compare" e1 e2
          ((e1,e2)::rest) ->
              AST.silentCase (BinaryOp "compare" e1 e2)
                  [ (ConsP "EQ" [], structuralCmpList rest)
                  , (VarP "c", Var "c")
                  ]
                  

-- compare constructor tags
-- special case for lists, otherwise compare alphabetically
compareCons : Tag -> Tag -> Order
compareCons t1 t2
    = case (t1,t2) of
          ("[]", ":") ->
              LT
          (":", "[]") ->
              GT
          _ ->
              compare t1 t2

-- build a case over a comparision result
-- shortcircuiting the the scrutinee is knowno
orderingCase : Expr -> Expr -> Expr -> Expr -> Expr
orderingCase expr ltBranch eqBranch gtBranch
    = case expr of
          Error msg ->
              Error msg
          Cons "LT" [] ->
              ltBranch
          Cons "EQ" [] ->
              eqBranch
          Cons "GT" [] ->
              gtBranch
          _ ->
              AST.silentCase expr [ (ConsP "LT" [], ltBranch)
                        , (ConsP "EQ" [], eqBranch)
                        , (ConsP "GT" [], gtBranch) ]
          

                  
-- convert a boolean to an AST expression             
compareOp : Bool -> Expr
compareOp c
    = if c then AST.trueCons else AST.falseCons

      
                          
orderingOp : Order -> Expr
orderingOp c
    = case c of
          LT -> AST.Cons "LT" []
          EQ -> AST.Cons "EQ" []
          GT -> AST.Cons "GT" []
                  
-----------------------------------------------------------------------
-- reduction to full normal form
-- Note: this destroys sharing in normal forms
-----------------------------------------------------------------------
deepEval : Expr -> Heap -> Stack -> Conf
deepEval expr heap stack 
    =  case outermostCont expr of
           Just (ctx, expr1) ->
               (heap, E expr1, Continue expr ctx::stack)
           Nothing ->
               (heap, E expr, stack)
                 

-- find the next outermost continuation for full evaluation
-- i.e. a context and subexpression
outermostCont : Expr -> Maybe (ExprCtx, Expr)
outermostCont expr
    = outermostRedex expr |>
      Maybe.andThen (\ctx -> ctx.getOption expr |>
                    Maybe.andThen (\expr1 -> Just (ctx, expr1)))

                   
-- determine the outermost reduction context
outermostRedex : Expr -> Maybe ExprCtx
outermostRedex expr
    = case expr of
          Cons tag args ->
              outermostRedexArgs tag 0 args
          _ ->
              Nothing

outermostRedexArgs : Tag -> Int -> List Expr -> Maybe ExprCtx
outermostRedexArgs tag i args
    = case args of
          [] ->
              Nothing
          (arg1::rest) ->
              if isWhnf arg1 then
                  case outermostRedex arg1 of
                      Nothing ->
                          outermostRedexArgs tag (i+1) rest
                      Just ctx1 ->
                          Just (Monocle.compose (Context.cons tag i) ctx1)
              else
                  Just (Context.cons tag i)
                  

-------------------------------------------------------------------------
-- initialize a heap with bindings for primitive operations
-------------------------------------------------------------------------
initializeHeap : Heap -> Heap
initializeHeap heap
    = Heap.insertFromList heap <|
      [ (":", AST.lambda Nothing (Match (VarP "x")
                                      (Match (VarP "y")
                                           (Return (Cons ":" [Var "x",Var "y"])
                                                Nothing)
                                      ))) 
      ] ++
      List.map infixOp [ "+", "-", "*", "<", ">", "<=", ">=",
                             "div", "mod", "compare" ]
         ++
      List.map prefixOp ["negate", "ord", "chr", "toUpper", "toLower",
                         "isLower", "isUpper", "isAlpha", "isDigit", "isAlphaNum"]


prefixOp :  Name -> (Name, Expr)
prefixOp op
    = (op, AST.lambda Nothing
           (Match (VarP "x") (Return (UnaryOp op (Var "x")) Nothing)))

          
infixOp : Name -> (Name, Expr)
infixOp op = (op, AST.lambda Nothing
                   (Match (VarP "x")
                       (Match (VarP "y")
                           (Return (BinaryOp op (Var "x") (Var "y"))
                                 Nothing)
                        )))

          
              
--
-- the  start configuration for fully evaluating an expression
--
start : Heap -> Expr -> Conf
start heap0 expr
    = (initializeHeap heap0, E expr, [DeepEval])

-- size expansion limit;
-- this is used to prevent infinite evaluation
sizeLimit : Int
sizeLimit = 100
      
--            
-- a labelled transition step ignoring silent transitions
-- 1st argument is the set of functions to skip 
next : Set Name -> Conf -> Maybe (Conf, Info)
next skipped conf0
    = let size0 = confSize conf0
      in 
          nextAux skipped (size0 + sizeLimit) conf0

-- worker function with an iteration limit
-- the 2nd argument is limit counter for "silent" transitions
nextAux :  Set Name -> Int -> Conf -> Maybe (Conf, Info)
nextAux skipped limit conf0
    = if confSize conf0 < limit then
          case transition skipped conf0 of
              Nothing ->
                  Nothing
              Just conf1 ->
                  case justification skipped conf0 of
                      Just info ->
                          Just (conf1, info)
                      Nothing ->
                          nextAux skipped limit conf1
      else
          Just (conf0, "continue evaluation?")

                          


-- size metric for a configuration
confSize : Conf -> Int
confSize (heap, control, stack)
    = case control of
          E expr ->
              exprSize expr + stackSize stack
          _ ->
              stackSize stack
              
-- compute normal form expression sizes
exprSize : Expr -> Int
exprSize expr
    = case expr of
          Cons tag args ->
              1 + List.sum (List.map exprSize args)
          _ ->
              1

stackSize : Stack -> Int
stackSize stk = List.sum (List.map contSize stk)

contSize : Cont -> Int
contSize cont
    = case cont of
          ContBinary _ expr ->
              exprSize expr
          RetBinary _ expr ->
              exprSize expr
          Continue expr _ ->
              exprSize expr
          _ ->
              1

              
-- justification for a transition step 
justification : Set Name -> Conf -> Maybe Info
justification skip (heap, control, stack)
    = case (control, stack) of
         (E v1, ((RetBinary op v2)::_)) ->
             if isWhnf v1 && not (Set.member op skip) then 
                 Just ("primitive " ++ op)
             else
                 Nothing
         (E w, ((RetUnary op)::_)) ->
             if isWhnf w && not (Set.member op skip) then 
                 Just ("primitive " ++ op)
             else
                 Nothing
{-
         (E w, Update x::rest) ->
             if isWhnf w then 
                 Just ("updated " ++ x)
             else
                 Nothing
-}
         (M (Return expr info) [], MatchEnd Skipped::_) ->
             Nothing
         (M (Return expr info) [], MatchEnd NotSkipped::_) ->
             info

         (E (Error msg), _) ->
             Just "runtime error"

         (M Fail _, MatchEnd _::_) ->
             Just "pattern match failure"

         _ ->
             Nothing

{-
showPrim2 : Name -> Expr -> Expr -> String
showPrim2 op v1 v2
    = case (v1,v2) of
          (Number x1, Number x2) ->
              String.fromInt x1 ++ op ++ String.fromInt x2
          (Char x1, Char x2) ->
              String.fromChar x1 ++ op ++ String.fromChar x2
          _ ->
              op


showPrim1 : Name -> Expr -> String
showPrim1 op v1
    = case  v1 of
          Number x1 ->
              op ++ " " ++ String.fromInt x1
          Char x1 ->
              op ++ " " ++ String.fromChar x1
          _ ->
              op
-}              
                  
