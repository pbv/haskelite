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
    | RetPrim2 Name Int
      -- for full normal form evaluation
    | DeepEval Expr ExprCtx
      

      
isWhnf : Expr -> Bool
isWhnf expr =
    case expr of
        Lam _ m ->
            AST.matchingArity m > 0
        Number _ ->
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
          (heap,  E (App e1 e2), stack) ->
              Just (heap, E e1, PushArg e2::stack)
                  
          (heap, E (Lam optname m), stack) ->
              if AST.matchingArity m == 0 then
                  Just (heap, M m [], MatchEnd::stack)
              else
                  case stack of
                      PushArg e1::rest ->
                          -- check if we neeed to update the
                          -- result of evaluation
                          if isVar e1 || isWhnf e1 then
                              -- no update
                              Just (heap, E (Lam optname (Arg e1 m)), rest)
                          else
                              -- create new indirection to the expression
                              let
                                  (loc, heap1) = Heap.newIndirection heap e1
                              in
                                  Just (heap1, E (Lam optname (Arg (Var loc) m)), rest)
                      Update y::rest ->
                          let
                              heap1 = Heap.update y (Lam optname m) heap
                          in 
                              Just (heap1, E (Lam optname m), rest)
                      _ ->
                          Nothing

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
                  
          (heap, E (Number v1), (RetPrim1 op e2)::stack) ->
              Just (heap, E e2, (RetPrim2 op v1)::stack)

          (heap, E (Number v2), (RetPrim2 op v1)::stack) ->
              case applyPrimitive op v1 v2 of
                  Just result ->
                      Just (heap, E result, stack)
                  _ ->
                      Just (heap, E Error, stack)
                  
          -- update variable
          (heap, E w, Update y::stack) ->
              if isWhnf w then
                  let
                      heap1 = Dict.insert y w heap
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
                  Just (heap, M (matchCons es ps m1) args, stack) 
              else
                  Just (heap, M Fail [], stack)

          (heap, E w, (PushPat args (BangP x) m1)::stack) ->
              let
                  m2 = AST.applyMatchSubst (Dict.singleton x w) m1
              in
                  Just (heap, M m2 args, stack)                          

          (heap, E (Number n), (PushPat args (NumberP k) m1)::stack) ->
              if n == k then
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
              Just (heap, E Error, stack)
                  
          -- deal with alternatives
          (heap, M (Alt m1 m2) args, stack) ->
              Just (heap, M m1 args, PushAlt args m2::stack)

          -- deal with arguments
          (heap, M (Arg e m1) args, stack) ->
              Just (heap, M m1 (e::args), stack)

          -- deep evaluation
          (heap, E w, (DeepEval expr ctx)::stack) ->
              if isWhnf w then
                  deepEval heap (ctx.set w expr) stack
              else
                  Nothing
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
              

applyPrimitive : Name -> Int -> Int -> Maybe Expr
applyPrimitive op v1 v2
    = case op of
          "+" -> Just (Number (v1 + v2))
          "-" -> Just (Number (v1 - v2))
          "*" -> Just (Number (v1 * v2))
          "div" ->
              Just (if v2 /= 0 then
                        Number (v1 // v2)
                    else
                        Error)
          "mod" ->
              Just (if v2 /= 0 then
                        Number (modBy v2 v1)
                    else
                        Error)
          "==" ->
              Just (compareOp (v1 == v2))
          ">" ->
              Just (compareOp (v1 > v2))
          ">=" ->
              Just (compareOp (v1 >= v2))
          "<" ->
              Just (compareOp (v1 < v2))
          "<=" ->
              Just (compareOp (v1 <= v2))
          _ ->
              Nothing

compareOp : Bool -> Expr
compareOp c = if c then AST.trueCons else AST.falseCons

-----------------------------------------------------------------------
-- reduction to full normal form
-----------------------------------------------------------------------
deepEval : Heap -> Expr -> Stack -> Maybe Conf
deepEval heap expr stack
    = case outermostRedex expr of
          Just ctx ->
              ctx.getOption expr |>
              Maybe.andThen
                  (\expr1 ->
                       Just (heap, E expr1, DeepEval expr ctx::stack))
          Nothing ->
              Just (heap, E expr, stack)


                  
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
-- a heap with lambda matchings for primitive operations
-- to allow partial applications
-------------------------------------------------------------------------
heap0 : Heap
heap0
    = Dict.fromList <|
      List.map  (\op -> (op, Lam (Just op)
                             (Match (VarP "x")
                              (Match (VarP "y")
                               (Return (InfixOp op (Var "x") (Var "y"))
                                    ("apply primitive " ++ op)
                               ))))) <|
      [ "+", "-", "*", "<", ">", "<=", ">=", "div", "mod" ]
          

              
--
-- the  start configuration for fully evaluating an expression
--
start : Heap -> Expr -> Conf
start heap expr
    = let
        heap1 = Dict.union heap0 heap
      in
        (heap1, E expr, [DeepEval expr Context.empty])

--            
-- a labelled transition step ignoring silent transitions
--
next : Conf -> Maybe (Conf, Info)
next conf0
    = nextW 100 conf0

-- worker function with an iteration limit
nextW : Int -> Conf -> Maybe (Conf, Info)
nextW iters conf0
    = if iters > 0 then
          case transition conf0 of
              Nothing ->
                  Nothing
              Just conf1 ->
                  case justification conf0 of
                      Just info ->
                          Just (conf1, info)
                      Nothing ->
                          nextW (iters-1) conf1
      else
          Just (conf0, "<loop>")

                          
                          
-- justification for a transition step 
justification : Conf -> Maybe String
justification (heap, control, stack)
    = case (control, stack) of
         (E (Number v1), (RetPrim2 op v2::_)) ->
             Just ("primitive " ++ op)
         (M (Return expr info) [], MatchEnd::_) ->
             Just info
         (M Fail [], MatchEnd::_) ->
             Just "pattern match failure"
         _ ->
             Nothing
                 
            
                  
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
       case next conf of
           Nothing ->
               ()
           Just (conf1,_) ->
               transitions_ (n+1) conf1

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
