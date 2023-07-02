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
                     Subst)
import Dict exposing (Dict)
import Pretty exposing (StringBuilder)
import DList
import Heap exposing (Heap)
import Debug


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
      

isWhnf : Expr -> Bool
isWhnf expr =
    case expr of
        Lam _ m ->
            AST.matchingArity m > 0
        Number _ ->
            True
        ListLit _ ->
            True
        TupleLit _ ->
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
-- a single step transition of the machine
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
                          -- check if neeed to memoize the
                          -- result of evaluation
                          if isVar e1 || isWhnf e1 then
                              Just (heap, E (Lam optname (Arg e1 m)), rest)
                          else
                              -- create new indirection to the expression
                              let
                                  (loc, heap1) = Heap.newIndirection heap e1
                              in
                                  Just (heap1, E (Lam optname (Arg (Var loc) m)), rest)
                      Update y::rest ->
                          let
                              heap1 = Dict.insert y (Lam optname m) heap
                          in 
                              Just (heap1, E (Lam optname m), rest)
                      _ ->
                          Nothing

          (heap, E (Var y), stack) ->
              case Dict.get y heap of
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
                      
          -- match a constructor
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
          
                      
          -- special cases for list patterns                
          (heap, E (ListLit []), (PushPat args (ConsP ":" [p1,p2]) m1)::stack) ->
               Just (heap, M Fail [], stack)                    
                      
          (heap, E (ListLit es), (PushPat args (ListP ps) m1)::stack) ->
              if List.length es == List.length ps then
                  Just (heap, M (matchCons es ps m1) args, stack)
              else
                  Just (heap, M Fail [], stack)

          (heap, E (ListLit (e1::es)), (PushPat args (ConsP ":" [p1,p2]) m1)::stack) ->
              Just (heap,
                        M (Match p1
                               (Arg (ListLit es) (Match p2 m1))) (e1::args),
                        stack)

          (heap, E (Cons ":" [e1,e2]), (PushPat args (ListP (p1::ps)) m1)::stack) ->
              Just (heap, M (Match p1
                                 (Arg e2 (Match (ListP ps) m1))) (e1::args),
                        stack)
                  
          -- successful match: return an expression
          (heap, M (Return expr info) args, MatchEnd::stack) ->
              Just (heap, E (applyArgs expr args), stack)

          (heap, M (Return expr info) args, (PushAlt _ _)::stack) ->
              Just (heap, M (Return expr info) args, stack)

          -- failing match
          (heap, M Fail _, (PushAlt args m)::stack) ->
              Just (heap, M m args, stack)

          -- deal with alternatives
          (heap, M (Alt m1 m2) args, stack) ->
              Just (heap, M m1 args, PushAlt args m2::stack)

          -- deal with arguments
          (heap, M (Arg e m1) args, stack) ->
              Just (heap, M m1 (e::args), stack)
                  
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

              
-- debugging function
transitions : Conf -> ()
transitions conf = transitions_ 0 conf
              
transitions_ : Int -> Conf -> ()
transitions_ n conf
    = let
        _ = Debug.log (String.fromInt n) (getControl conf, getStack conf)
      in
       case next conf of
           Nothing ->
               ()
           Just conf1 ->
               transitions_ (n+1) conf1

next : Conf -> Maybe Conf
next conf
    = case transition conf of
          Nothing ->
              Nothing
          Just conf1 ->
              if observe conf1 then
                  Just conf1
              else
                  next conf1

                          
-- check whether a configuration should be shown to the user
observe : Conf -> Bool
observe (heap, control, stack) 
    = case (control,stack) of
          (E (Var _),  _) ->
              False
          (E (App _ _), _) ->
              False
          (E (Lam _ m), _) ->
              AST.matchingArity m == 0
          (E w, (Update _::_)) ->
               False
          (E _, _) ->
              True
          (M (Return _ _) _, (MatchEnd::_)) ->
              True
          (M Fail _, (MatchEnd::_)) ->
              True
          (M _ _, _) ->
              False

{-
explainConf : Conf -> Maybe String
explainConf (heap, control, stack)
    = case control of
          M (Return expr info) _ ->
              Just info
          _ ->
              Nothing
-}
                  
prettyConf : Conf -> Maybe String
prettyConf (heap, control, stack)
   = case control of
         (E expr) ->
             Just <|
                 Pretty.toString <|
                 prettyCont heap stack <|
                 Pretty.prettyExpr_ heap 0 expr
         _ ->
             Nothing

                 
-- convert a continuation stack into an pretty expression
prettyCont : Heap -> Stack -> StringBuilder -> StringBuilder
prettyCont heap stack acc
    = case stack of
          [] ->
              acc
          (Update _::rest) ->
              prettyCont heap rest acc
          (PushArg arg::rest) ->
              let
                  acc1 = DList.append acc
                         (DList.cons " " (Pretty.prettyExpr_ heap 1 arg))
              in
                  prettyCont heap rest acc1
          (RetPrim1 op e2::rest) ->
              let acc1 = DList.append
                         acc (DList.cons op
                                  (Pretty.prettyExpr_ heap 1 e2))
              in 
                 prettyCont heap rest acc1
          (RetPrim2 op v::rest) ->
              let acc1 = DList.append
                         (DList.singleton (String.fromInt v))
                              (DList.cons op acc)
              in
                  prettyCont heap rest acc1
          (_::rest) ->
              prettyCont heap rest acc
--------------------------------------------------------------------



--------------------------------------------------------------------
                          
example0 : Conf
example0 = (Dict.empty, E (InfixOp "+" (Number 1) (Number 2)), [])

example1 : Conf
example1 =
    let
        heap = Dict.singleton "x" (InfixOp "+" (Number 1) (Number 2))
        stack = []
    in 
        (heap, E (Var "x"), stack)


example3 : Conf
example3 =
   let tl =  Lam (Just "tail")
             (Match (ConsP ":" [VarP "x", VarP "xs"])
                      (Return (Var "xs") "tail (x:xs) = xs"))
       e = App tl (ListLit [Number 1, Number 2, Number 3])
   in (Dict.empty, E e, [])

example4 : Conf
example4
    = let
        expr = App (Lam Nothing
                        (Match (ConsP ":" [VarP "h",VarP "t"])
                             (Return (Var "h") "head")))
               (ListLit [])
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
                         (Match (ListP [])
                              (Return (Number 0) "sum [] = 0"))))
        control = E (App (Var "sum") (ListLit [Number 1,Number 2,Number 3]))
        stack = []
    in
        (heap, control, stack)

example6 : Conf
example6 =
    let
        heap = Dict.singleton "fact"
               (Lam (Just "fact")
                    (Alt (Match (NumberP 0)
                              (Return (Number 1) "fact 0 = 1"))
                         (Match (VarP "n")
                              (Return (InfixOp "*" (Var "n") (App (Var "fact") (InfixOp "-" (Var "n") (Number 1)))) "fact n = n * fact (n-1)"))))
        control = E (App (Var "fact") (Number 5))
        stack = []
    in
        (heap, control, stack)
           
         
example7 : Conf
example7 =
    let heap = Dict.singleton "factAcc"
               (Lam (Just "factAcc")
                    (Alt (Match (NumberP 0)
                              (Match (VarP "acc")
                                   (Return (Var "acc") "factAcc 0 acc = acc")))
                         (Match (VarP "n")
                              (Match (VarP "acc")
                                   (Return (App (App (Var "factAcc")
                                                     (InfixOp "-" (Var "n") (Number 1)))
                                                (InfixOp "*" (Var "n") (Var "acc"))) "factAcc n acc = factAcc (n-1) (n*acc)")))))
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
               
