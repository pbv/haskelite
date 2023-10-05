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
import Dict exposing (Dict)
import Heap exposing (Heap)
import Context exposing (ExprCtx)
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
    | MatchEnd 
    | PushAlt ArgStack Matching
    | PushPat ArgStack Pattern Matching
      -- continuations for primitive operations
    | RetPrim1 Name Expr
    | RetPrim2 Name Expr
    | RetPrim3 Name
      -- for full normal form evaluation
    | DeepEval
    | Continue Expr ExprCtx
      

      
isWhnf : Expr -> Bool
isWhnf expr =
    case expr of
        Lam 0 _ m ->     --- AST.matchingArity m == 0
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
--
transition : Conf -> Maybe Conf
transition conf
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
              Just (heap, M m [], MatchEnd::stack)

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
              Just (heap, M (AST.translateCase e1 alts) [], MatchEnd::stack)
                      
          -- if-then-else
          (heap, E (IfThenElse e1 e2 e3), stack) ->
              Just (heap, M (AST.translateIfThenElse e1 e2 e3) [], MatchEnd::stack)

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
          (heap, E (InfixOp op e1 e2), stack) ->
              Just (heap, E e1, (RetPrim1 op e2)::stack)
                  
          (heap, E (PrefixOp op e1), stack) ->
              Just (heap, E e1, (RetPrim3 op)::stack)

          -- the remaining expression evaluation rules
          -- should apply only to whnfs
          (heap, E w1, (RetPrim1 op e2)::stack) ->
              Just (heap, E e2, (RetPrim2 op w1)::stack)
          (heap, E w2, (RetPrim2 op w1)::stack) ->
              Just (heap, E (applyBinPrimitive op w1 w2), stack)

          (heap, E w, (RetPrim3 op)::stack) ->
              Just (heap, E (applyUnaryPrim op w), stack)
                  
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
          (heap, M (Return expr info) args, MatchEnd::stack) ->
              Just (heap, E (applyArgs expr args), stack)

          (heap, M (Return expr info) args, (PushAlt _ _)::stack) ->
              Just (heap, M (Return expr info) args, stack)

          -- failing matches
          (heap, M Fail _, (PushAlt args m)::stack) ->
              Just (heap, M m args, stack)

          (heap, M Fail _, MatchEnd::stack) ->
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
              -- if isWhnf w then
              Just (deepEval w heap stack)
              -- else
              --    Nothing
                  
          -- NB: this does not preserve sharing
          (heap, E w, (Continue expr ctx)::stack) ->
              -- if isWhnf w then
              Just (deepEval (ctx.set w expr) heap stack)
              -- else
              --    Nothing
                      
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
applyBinPrimitive : Name -> Expr -> Expr -> Expr
applyBinPrimitive op e1 e2
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
          ("==", _, _) ->
              structuralEq e1 e2
          ("/=", _, _) ->
              App (Var "not") (structuralEq e1 e2)
          ("compare", _, _) ->
              case (compareExpr e1 e2) of
                  Ok rel -> orderingOp rel
                  Err msg -> Error (AST.stringLit msg)
          (">", _, _) ->
              case compareExpr e1 e2 of
                  Ok GT -> AST.trueCons
                  Ok _ -> AST.falseCons
                  Err msg -> Error (AST.stringLit msg)
          (">=", _, _) ->
              case compareExpr e1 e2 of
                  Ok LT -> AST.falseCons
                  Ok _ -> AST.trueCons
                  Err msg -> Error (AST.stringLit msg)
          ("<", _, _) ->
              case compareExpr e1 e2 of
                  Ok LT -> AST.trueCons
                  Ok _ -> AST.falseCons
                  Err msg -> Error (AST.stringLit msg)
          ("<=", _, _) ->
              case compareExpr e1 e2 of
                  Ok GT -> AST.falseCons
                  Ok _ -> AST.trueCons
                  Err msg -> Error (AST.stringLit msg)
          _ ->
              Error (AST.stringLit "invalid primitive arguments")

-- apply a unary primitive
applyUnaryPrim : Name -> Expr -> Expr
applyUnaryPrim op e
    = case (op, e) of
          ("negate", Number v) ->
              Number (-v)
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


-- polymorphic structural equality
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
              if c1 == c2 && List.length args1 == List.length args2 then
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
              InfixOp "==" e1 e2
          ((e1,e2)::rest) ->
              App (App (Var "&&") (InfixOp "==" e1 e2)) (structuralEqList rest)

-- convert a boolean to an AST expression             
compareOp : Bool -> Expr
compareOp c
    = if c then AST.trueCons else AST.falseCons

      
compareExpr : Expr -> Expr -> Result String Order
compareExpr e1 e2
    = case (e1, e2) of
          (Number v1, Number v2) ->
              Ok (compare v1 v2)
          (Char v1, Char v2) ->
              Ok (compare v1 v2)
          (Cons _ _, Cons _ _) ->
              Err "can't compare constructors"
          (Lam _ _ _, Lam _ _ _) ->
              Err "can't compare functions"
          _ ->
              Err "invalid arguments to comparison"
                          
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
      , ("negate", AST.lambda Nothing (Match (VarP "x")
                                    (Return (PrefixOp "negate" (Var "x")) Nothing)))
      , ("compare", AST.lambda Nothing (Match (VarP "x")
                                            (Match (VarP "y")
                                                 (Return (InfixOp "compare" (Var "x") (Var "y")) Nothing))))
      ] ++
      List.map binop [ "+", "-", "*", "<", ">", "<=", ">=", "div", "mod" ]  

          
binop : Name -> (Name, Expr)
binop op = (op, AST.lambda Nothing
                   (Match (VarP "x")
                       (Match (VarP "y")
                           (Return (InfixOp op (Var "x") (Var "y"))
                                 Nothing)
                        )))

          
              
--
-- the  start configuration for fully evaluating an expression
--
start : Heap -> Expr -> Conf
start heap0 expr
    = (initializeHeap heap0, E expr, [DeepEval])

--            
-- a labelled transition step ignoring silent transitions
--
next : Conf -> Maybe (Conf, Info)
next conf0
    = nextAux 100 conf0

-- worker function with an iteration limit
-- the first argument is limit counter for "silent" transitions
nextAux :  Int -> Conf -> Maybe (Conf, Info)
nextAux iters conf0
    = if iters > 0 then
          case transition conf0 of
              Nothing ->
                  Nothing
              Just conf1 ->
                  case justification conf0 of
                      Just info ->
                          Just (conf1, info)
                      Nothing ->
                          nextAux (iters-1) conf1
      else
          Just (conf0, "cyclic definition?")

                          
                          
-- justification for a transition step 
justification : Conf -> Maybe Info
justification (heap, control, stack)
    = case (control, stack) of
         (E v1, (RetPrim2 op v2::_)) ->
             if isWhnf v1 then 
                 Just ("primitive " ++ showPrim2 op v2 v1)
             else
                 Nothing
         (E v1, (RetPrim3 op::_)) ->
             if isWhnf v1 then 
                 Just ("primitive " ++ showPrim1 op v1)
             else
                 Nothing
{-
         (E w, Update x::rest) ->
             if isWhnf w then 
                 Just ("updated " ++ x)
             else
                 Nothing
-}
         (M (Return expr info) [], MatchEnd::_) ->
             info
         (E (Error msg), _) ->
             Just "runtime error"

         (M Fail _, MatchEnd::_) ->
             Just "pattern match failure"

         _ ->
             Nothing


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
              
                  
            
--------------------------------------------------------------------
-- examples for debugging 
-------------------------------------------------------------------
{-
-- debugging function
transitions : Conf -> ()
transitions conf = transitions_ 0 conf
              
transitions_ : Int -> Conf -> ()
transitions_ n conf
    = let
        _ = Debug.log (String.fromInt n) (getControl conf, getStack conf )
      in
       case transition conf of
           Nothing ->
               ()
           Just conf1 ->
               transitions_ (n+1) conf1
-}

{-
example0 : Conf
example0 = (heap0, E (InfixOp "*" (Number 1) (Number 2)), [])

example1 : Conf
example1 =
    let
        heap = Dict.singleton "square"
               (Lam (Just "square")
                    (Match (VarP "x") (Return (InfixOp "*" (Var "x") (Var "x"))
                                           "square x = x*x")))
        stack = []
    in 
        (heap, E (App (Var "square") (Number 5)), stack)


example3 : Conf
example3 =
   let tl =  Lam (Just "tail")
             (Match (ConsP ":" [VarP "x", VarP "xs"])
                      (Return (Var "xs") "tail (x:xs) = xs"))
       e = App tl (AST.listLit [Number 1,Number 2,Number 3])
   in (Dict.empty, E e, [])

example4 : Conf
example4
    = let
        expr = App (Lam Nothing
                        (Match (ConsP ":" [VarP "h",VarP "t"])
                             (Return (Var "h") "head")))
               (AST.listLit [])
    in (Dict.empty, E expr, []) 
    


example5 : Conf
example5 =
    let
        heap = Dict.singleton "sum"
               (Lam (Just "sum")
                    (Alt (Match (ConsP ":" [VarP "x",VarP "xs"])
                              (Return (InfixOp "+"
                                           (Var "x")
                                           (App (Var "sum") (Var "xs")))
                                           "sum (x:xs) = x + sum xs"))
                         (Match (ConsP "[]" [])
                              (Return (Number 0) "sum [] = 0"))))
        control = E (App (Var "sum") (AST.listLit [Number 1,Number 2,Number 3]))
        stack = []
    in
        (heap, control, stack)

example6 : Conf
example6 =
    let
        heap = Dict.singleton "fact"
               (Lam (Just "fact")
                   (Alt (Match (VarP "n")
                             (Arg (InfixOp ">" (Var "n") (Number 0))
                                  (Match (ConsP "True" [])
                                       (Return (InfixOp "*" (Var "n") (App (Var "fact") (InfixOp "-" (Var "n") (Number 1)))) "fact n | n>0 = n*fact (n-1)"))))
                        (Match (VarP "n")
                             (Return (Number 1) "fact n | otherwise = 1"))))
        control = E (App (Var "fact") (Number 5))
        stack = []
    in
        (heap, control, stack)
           
         
example7 : Conf
example7 =
    let heap = Dict.singleton "factAcc"
               (Lam (Just "factAcc")
                    (Alt (Match (VarP "n")
                              (Match (VarP "acc")
                                   (Arg (InfixOp ">" (Var "n") (Number 1))
                                        (Match (ConsP "True" [])
                                             (Return (App (App (Var "factAcc") (InfixOp "-" (Var "n") (Number 1))) (InfixOp "*" (Var "n") (Var "acc"))) "factAcc n acc | n>0 = facAcc (n-1) (n*acc)")))))
                         (Match DefaultP
                              (Match (VarP "acc")
                                   (Return (Var "acc") "factAcc _ acc = acc")))))
        control = E (App (App (Var "factAcc") (Number 5)) (Number 1))
        stack = []
    in
        (heap, control, stack)


example8
    = let heap = Dict.fromList
                 [ ("square", Lam (Just "square")
                        (Match (VarP "x")
                             (Return (InfixOp "*" (Var "x") (Var "x"))
                                  "square x = x*x")))
                 ]
      in
          (heap, E (App (Var "square") (App (Var "square") (Number 5))), [])
               

example9
    = let heap = Dict.fromList
                 [ ("revacc", Lam (Just "revacc")
                        (Alt
                         (Match (ConsP ":" [VarP "x",VarP "xs"])
                              (Match (VarP "acc")
                                   (Return
                                        (App (App (Var "revacc") (Var "xs"))  (Cons ":" [Var "x",Var "acc"])) "revacc (x:xs) acc = revacc xs (x:acc)")))
                         (Match (ConsP "[]" [])
                              (Match (VarP "acc")
                                   (Return (Var "acc")
                                        "revacc [] acc = acc")))
                        ))
                 ]
      in (heap, E (App (App (Var "revacc") (AST.listLit [Number 1, Number 2, Number 3])) (AST.listLit [])), [])
              


example10
    = let heap = Dict.fromList
                 [ ("double", Lam (Just "double")
                      (Alt
                       (Match (ConsP ":" [VarP "x", VarP "xs"])
                            (Return
                                 (Cons ":"
                                 [InfixOp "*" (Number 2) (Var "x"),
                                      App (Var "double") (Var "xs")])
                                 "double-1"))
                       (Match (ConsP "[]" [])
                            (Return (Cons "[]" []) "double-2"))
                           ))
                     ]

          expr = (App (Var "double") (AST.listLit [Number 1, Number 2, Number 3]))
      in
          (heap, E expr, [DeepEval expr Context.empty])
-}

{-
example11
    = (Heap.empty,
           E (Case (InfixOp "==" (Number 1) (Number 2))
                  [(ConsP "True" [], Number 42)]),
           
                      [])
-}

{-
-- extra debugging stuff                 
observe : a -> b -> b
observe x y
    = let
        _ = Debug.log ">>>" x
      in  y
-}
