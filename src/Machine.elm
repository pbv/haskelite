{- 
  A Lazy abstract machine based on the PMC calculus

  Pedro Vasconcelos, 2023
-}
module Machine exposing (..)

import AST exposing (..)
import Machine.Types exposing (..)
import Machine.Heap exposing (Heap)
import Machine.Heap as Heap
import Context exposing (ExprCtx)
import Dict exposing (Dict)
import Shows

import Monocle.Optional as Monocle

-- check if an expression is in weak head normal form      
isWhnf : Expr -> Bool
isWhnf expr =
    case expr of
        Lam arity _ _ ->
            arity > 0                
        Number _ ->
            True
        Char _ ->
            True
        Cons flag _ _ ->
            flag
        _ ->
            False

-- check if an expression is *atomic*, i.e.
-- if we can safely share it without losing laziness
isAtomic : Expr -> Bool
isAtomic expr =
    case expr of
        Var _ ->
            True
        Number _ ->
            True
        Char _ ->
            True
        Cons _ _ [] ->
            True
        _ ->
            False


-- normalize an argument, possibly allocating a new indirection                
normalizeArg : Heap -> Expr -> (Expr, Heap)
normalizeArg heap expr
    = if isAtomic expr then
          (expr, heap)
      else
          let (name, heap1) = Heap.newIndirection heap expr
          in (Var name, heap1)
                             

-- normalize a list of arguments 
normalizeArgs : Heap -> List Expr -> (List Expr, Heap)
normalizeArgs heap args
    = case args of
          [] ->
              ([], heap)
          (arg::rest) ->
              let (exp, heap1) = normalizeArg heap arg
                  (exps, heap2) = normalizeArgs heap1 rest
              in (exp::exps, heap2)

                           
--                
-- a small-step transition of the machine
-- first argument is the set of functions to skip over
transition : Conf -> Maybe Conf
transition conf 
    = case conf of
          -- consume intermediate states used for pretty-printing
          (heap, E w, RetBinary _ _ _::stack) ->
              Just (heap, E w, stack)

          (heap, E w, RetUnary _ _::stack) ->
              Just (heap, E w, stack)
                  
          -- short circuit errors
          (heap, E (Exception msg), _::_) ->
              Just (heap, E (Exception msg), [])

          -- primitive to force full evaluation
          (heap, E (App (Var "force") e2), stack) ->
              Just (heap, E e2, DeepEval::stack)
          -- applications and constructors                  
          (heap,  E (App e1 e2), stack) ->
              let
                  (arg, heap1) = normalizeArg heap e2
              in 
                  Just (heap1, E e1, PushArg arg::stack)

          -- normalize constructor arguments
          (heap, E (Cons False c args), stack) ->
              let (args1, heap1) = normalizeArgs heap args
              in Just (heap1, E (Cons True c args1), stack)
                           
          -- add a new argument to constructor
          -- we know argument in PushArg has been normalized
          (heap, E (Cons True c args), PushArg arg::stack) ->
              Just (heap, E (Cons True c (args ++ [arg])), stack)
                  
          -- saturated lambda: go into matching evaluation
          (heap, E (Lam 0 optname m), stack) ->
              Just (heap, M m [], MatchEnd::stack)

          -- apply argument to non-saturated lambda
          (heap, E (Lam _ optname m), PushArg e1::rest) ->
              Just (heap, E (AST.lambda optname (Arg e1 m)), rest)

          -- local bindings
          (heap, E (Let binds e1), stack) ->
              let
                  (subst, heap1) = Heap.newBindings heap binds 
              in
                  Just (heap1, E (AST.applySubst subst e1), stack)

          -- case expressions
          (heap, E (Case e0 alts), stack) ->
              let
                  (e1, heap1) = normalizeArg heap e0
              in 
                  Just (heap1, M (translateCase e1 alts) [], MatchEnd::stack)
                      
          -- if-then-else
          (heap, E (IfThenElse e1 e2 e3), stack) ->
              Just (heap, M (translateIfThenElse e1 e2 e3) [], MatchEnd::stack)

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
                      Just (panic ("dangling variable: "++y))

          -- primitive operations
          (heap, E (BinaryOp op e1 e2), stack) ->
              Just (heap, E e1, (ContBinary1 op e2)::stack)

          (heap, E (UnaryOp "error" e1), stack) ->
              Just (heap, E e1, DeepEval::ContUnary "error"::stack)
                  
          (heap, E (UnaryOp op e1), stack) ->
              Just (heap, E e1, (ContUnary op)::stack)

          -- the remaining expression evaluation rules
          -- should apply only to whnfs
          (heap, E w1, (ContBinary1 op e2)::stack) ->
              Just (heap, E e2, (ContBinary2 op w1)::stack)

          (heap, E w2, (ContBinary2 op w1)::stack) ->
              case applyPrimitive2 op w1 w2 of
                  Exception msg ->
                      Just (heap, E (Exception msg), [])
                  result ->
                      Just (heap, E result, (RetBinary op w1 w2::stack))
                        
          (heap, E w, (ContUnary op)::stack) ->
              case applyPrimitive1 op w of
                  Exception msg ->
                      Just (heap, E (Exception msg), [])
                  result ->
                      Just (heap, E result, (RetUnary op w::stack))
                  
          -- update a variable
          -- at this point we know w must be in wnhf
          (heap, E w, Update y::stack) ->
              let
                  heap1 = Heap.update y w heap
              in
                  Just (heap1, E w, stack)

          -- ignore an argument
          (heap, M (Match DefaultP m1) (arg1::args), stack) ->
              Just (heap, M m1 args, stack)
                      
          -- bind a variable
          (heap, M (Match (VarP x) m1) (arg1::args), stack) ->
              let
                  (loc,heap1) = Heap.newIndirection heap arg1
                  m2 = AST.applyMatchSubst (Dict.singleton x loc) m1
              in
                  Just (heap1, M m2 args, stack)

          -- match as-patterns
          (heap, M (Match (AsP x pat) m1) args, stack) ->
              Just (heap, M (Match (VarP x) (Arg (Var x) (Match pat m1))) args, stack)

          -- match bang-patterns
          (heap, M (Match (BangP x) m0) (arg0::args), stack) ->
              let
                  (loc, heap1) = Heap.newIndirection heap arg0
                  m1 = AST.applyMatchSubst (Dict.singleton x loc) m0
              in 
                  Just (heap1, E (Var loc), (PushBang args m1)::stack)
                      
          -- match a constructor, number or char pattern
          (heap, M (Match patt m1) (arg1::args), stack) ->
              Just (heap, E arg1, (PushPat args patt m1)::stack)
                      
          -- decompose a constructor 
          (heap, E (Cons True c0 es), (PushPat args (ConsP c1 ps) m1)::stack) ->
              if c0 == c1 then
                  -- |es| == |ps| must hold because of well-typing  
                  let
                      (es1, heap1) = normalizeArgs heap es
                  in 
                      Just (heap1, M (matchCons es1 ps m1) args, stack) 
              else
                  Just (heap, M Fail [], stack)

          (heap, E w, (PushBang args m1)::stack) ->
              -- ignore the weak normal form because we have already named it
              Just (heap, M m1 args, stack) 

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
          (heap, M (Return expr info) args, MatchEnd::stack) ->
              Just (heap, E (applyArgs expr args), stack)

          (heap, M (Return expr info) args, (PushAlt _ _)::stack) ->
              Just (heap, M (Return expr info) args, stack)

          -- failing matches
          (heap, M Fail _, (PushAlt args m)::stack) ->
              Just (heap, M m args, stack)

          (heap, M Fail _, MatchEnd::stack) ->
              Just (heap, E (Exception "non-exaustive patterns"), [])
                  
          -- deal with alternatives
          (heap, M (Alt m1 m2) args, stack) ->
              Just (heap, M m1 args, PushAlt args m2::stack)

          -- deal with arguments
          (heap, M (Arg e m1) args, stack) ->
              Just (heap, M m1 (e::args), stack)

          -- deal with where bindings
          (heap, M (Where binds m1) args, stack) ->
              let
                  (s, heap1) = Heap.newBindings heap binds
              in
                  Just (heap1, M (AST.applyMatchSubst s m1) args, stack)

                  
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
                  -- truncate towards -infinity following the Haskell standard
                  let r = v1 // v2
                  in Number (if r<0 then 
                                 r-1
                             else
                                 r)
               else
                   Exception "division by zero"
          ("mod", Number v1, Number v2) ->
              if v2 /= 0 then
                   Number (modBy v2 v1)
               else
                   Exception "division by zero"
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
              Exception "invalid primitive arguments"


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
          ("show", Number n) ->
              AST.stringLit (String.fromInt n)
          ("error", str) ->
              Exception (AST.stringUnlit str)
          _  ->
              Exception "invalid primitive arguments"



-- polymorphic structural equality (==)
-- assumes arguments are already in whnf but *not* full normal form
-- produces an expression that is not in whnf
structuralEq :  Expr -> Expr -> Expr
structuralEq e1 e2
    = case (e1, e2) of
          (Number v1, Number v2) ->
              compareOp (v1 == v2)
          (Char c1, Char c2) ->
              compareOp (c1 == c2)
          (Cons _ c1 args1, Cons _ c2 args2) ->
              if c1 == c2 then
                  structuralEqList args1 args2
              else
                  AST.falseCons
          (Lam _ _ _, Lam _ _ _) ->
              Exception "can't compare functions"
          _ ->
              Exception "invalid arguments to equality"

structuralEqList : List Expr -> List Expr -> Expr
structuralEqList args1 args2
    = case (args1, args2) of
          ([], []) ->
              AST.trueCons
          ([e1],[e2]) ->
              BinaryOp "==" e1 e2
          (e1::rest1, e2::rest2) ->
              App (App (Var "&&") (BinaryOp "==" e1 e2)) (structuralEqList rest1 rest2)
          _ ->
              --  this case shouldn't happen because the constructor tags match
              Exception "shouldn't happen"



-- polymorphic structural comparison
structuralCmp : Expr -> Expr -> Expr
structuralCmp e1 e2
    = case (e1, e2) of
          (Number v1, Number v2) ->
              orderingOp (compare v1 v2)
          (Char c1, Char c2) ->
              orderingOp (compare c1 c2)
          (Cons _ c1 args1, Cons _ c2 args2) ->
              case compareCons c1 c2 of
                  EQ ->
                      structuralCmpList args1 args2
                  LT ->
                      Cons True "LT" []
                  GT ->
                      Cons True "GT" []
          (Lam _ _ _, Lam _ _ _) ->
              Exception "can't compare functions"
          _ ->
              Exception "invalid arguments to structuralCmp"

structuralCmpList : List Expr -> List Expr -> Expr
structuralCmpList args1 args2
    = case (args1, args2) of
          ([], []) ->
              Cons True "EQ" []
          ([e1],[e2]) ->
              BinaryOp "compare" e1 e2
          (e1::rest1, e2::rest2) ->
              Case (BinaryOp "compare" e1 e2)
                  [ (ConsP "EQ" [], structuralCmpList rest1 rest2, "EQ")
                  , (ConsP "LT" [], Cons True "LT" [], "LT")
                  , (ConsP "GT" [], Cons True "GT" [], "GT")
                  ]
          (_, _) ->
              Exception "shouldn't happen"
                  

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

-- build a case over a comparison result
-- shortcircuiting if the scrutinee is known
orderingCase : Expr -> Expr -> Expr -> Expr -> Expr
orderingCase expr ltBranch eqBranch gtBranch
    = case expr of
          Exception msg ->
              Exception msg
          Cons _ "LT" [] ->
              ltBranch
          Cons _ "EQ" [] ->
              eqBranch
          Cons _ "GT" [] ->
              gtBranch
          _ ->
              Case expr [ (ConsP "LT" [], ltBranch, "LT")
                        , (ConsP "EQ" [], eqBranch, "EQ")
                        , (ConsP "GT" [], gtBranch, "GT") ]
          

                  
-- convert a boolean to an AST expression             
compareOp : Bool -> Expr
compareOp c
    = if c then AST.trueCons else AST.falseCons

      
                          
orderingOp : Order -> Expr
orderingOp c
    = case c of
          LT -> AST.Cons True "LT" []
          EQ -> AST.Cons True "EQ" []
          GT -> AST.Cons True "GT" []
                  
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
          Cons False _ _ ->
              Just Context.empty
          Cons True _ args ->
              outermostRedexArgs 0 args
          _ ->
              Nothing

outermostRedexArgs : Int -> List Expr -> Maybe ExprCtx
outermostRedexArgs i args
    = case args of
          [] ->
              Nothing
          (arg1::rest) ->
              if isWhnf arg1 then
                  case outermostRedex arg1 of
                      Nothing ->
                          outermostRedexArgs (i+1) rest
                      Just ctx1 ->
                          Just (Monocle.compose (Context.cons i) ctx1)
              else
                  Just (Context.cons i)
                  
-- abort with an internal error
panic : String -> Conf
panic msg
    = (Heap.empty, E (Exception msg), [])
                      
-------------------------------------------------------------------------
-- initialize a heap with bindings for primitive operations
-------------------------------------------------------------------------
initializeHeap : Heap -> Heap
initializeHeap heap
    = Heap.insertFromList heap <|
      [ (":", AST.lambda (Just ":") (Match (VarP "x")
                                      (Match (VarP "y")
                                           (Return (Cons True ":" [Var "x",Var "y"])
                                                Nothing)
                                      )))
      ] ++
      List.map infixOp [ "+", "-", "*", "<", ">", "<=", ">=",
                             "div", "mod", "compare" ]
         ++
      List.map prefixOp ["negate", "ord", "chr", "toUpper", "toLower",
                         "isLower", "isUpper", "isAlpha", "isDigit", "isAlphaNum",
                         "show", "error" ]


prefixOp :  Name -> (Name, Expr)
prefixOp op
    = (op, AST.lambda (Just op)
           (Match (VarP "x") (Return (UnaryOp op (Var "x")) Nothing)))

          
infixOp : Name -> (Name, Expr)
infixOp op = (op, AST.lambda (Just op)
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
-- this is used to prevent loops in evaluation
sizeLimit : Int
sizeLimit = 100
      
--            
-- a labelled transition step ignoring silent steps
labelledTransition : Conf -> Maybe (Conf, Info)
labelledTransition conf0
    = let size0 = confSize conf0
      in  transition conf0 |> Maybe.andThen (labelledWorker (size0+sizeLimit))

-- worker function with an iteration limit
-- the 1st argument is a limit counter 
labelledWorker :  Int -> Conf -> Maybe (Conf, Info)
labelledWorker limit conf
    = if confSize conf < limit then
          case justification conf of
              Just info ->
                  Just (conf, info)
              Nothing ->
                  transition conf |> Maybe.andThen (labelledWorker limit)
      else
          Just (conf, "continue?")



-- size metric for a configuration
confSize : Conf -> Int
confSize (heap, control, stack)
    = case control of
          E expr ->
              exprSize heap expr + stackSize heap stack
          _ ->
              stackSize heap stack
              
-- compute normal form expression sizes
exprSize : Heap -> Expr -> Int
exprSize heap expr
    = case expr of
          Cons _ tag args ->
              1 + List.sum (List.map (exprSize heap) args)
          Var x ->
              -- follow the heap pointers at most once
              case Heap.get x heap of
                  Just expr1 ->
                      exprSize (Heap.delete x heap) expr1
                  Nothing ->
                      1
          _ ->
              1

stackSize : Heap -> Stack -> Int
stackSize heap stk = List.sum (List.map (contSize heap) stk)

contSize : Heap -> Cont -> Int
contSize heap cont
    = case cont of
          ContBinary1 _ expr ->
              exprSize heap expr
          ContBinary2 _ expr ->
              exprSize heap expr
          Continue expr _ ->
              exprSize heap expr
          _ ->
              1

              
-- justification for a transition step 
justification : Conf -> Maybe Info
justification (heap, control, stack)
    = case (control, stack) of
         (E w, ((RetBinary op e1 e2)::_)) ->
             Just (showPrim2 op e1 e2 w)
         (E w, ((RetUnary op e1)::_)) ->
             Just (showPrim1 op e1 w)
         (M (Return expr info) [], MatchEnd::_) ->
             info
                 
         (E (Exception msg), _) ->
             Just "runtime error"

         (M Fail _, MatchEnd::_) ->
             Just "pattern match failure"

         (E w, []) ->
             Just "final result"

         _ ->
             Nothing


showPrim2 : Name -> Expr -> Expr -> Expr -> String
showPrim2 op e1 e2 e3
    = if isWhnf e3 then
          Shows.showExpr (BinaryOp op e1 e2) ++ " = " ++  Shows.showExpr e3
      else
          "definition of " ++ op 

showPrim1 : Name -> Expr -> Expr -> String
showPrim1 op e1 e2
    = if isWhnf e2 then
          Shows.showExpr (UnaryOp op e1) ++ " = " ++ Shows.showExpr e2
      else
          "definition of " ++ op 


