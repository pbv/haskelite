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
import Debug

type alias Conf
    = (Heap, Control, Stack)

type alias Heap 
    = Dict Name Expr

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


getHeap : Conf -> Heap
getHeap (heap,_,_) = heap

getControl : Conf -> Control
getControl (_, control, _) = control

getStack : Conf -> Stack
getStack (_, _, stack) = stack           

last : List a -> Maybe a
last xs = case xs of
              [] -> Nothing
              [x] -> Just x
              (x::xs1) -> last xs1
                         
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
                          Just (heap, E (Lam Nothing (Arg e1 m)), rest)
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
                      let
                          heap1 = Dict.remove y heap
                      in
                          Just (heap1, E expr, Update y::stack)
                  _ ->
                      Nothing

          -- primitive operations
          (heap, E (InfixOp op e1 e2), stack) ->
              Just (heap, E e1, (RetPrim1 op e2)::stack)
                  
          (heap, E (Number v1), (RetPrim1 op e2)::stack) ->
              Just (heap, E e2, (RetPrim2 op v1)::stack)

          (heap, E (Number v2), (RetPrim2 op v1)::stack) ->
              case applyPrimitive op v1 v2 of
                  Just v ->
                      Just (heap, E (Number v), stack)
                  _ ->
                      Nothing
                  
          -- update variable
          (heap, E w, Update y::stack) ->
              if isWhnf w then
                  let
                      heap1 = Dict.insert y w heap
                  in
                      Just (heap1, E w, stack)
              else
                  Nothing

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
              

applyPrimitive : Name -> Int -> Int -> Maybe Int
applyPrimitive op v1 v2
    = case op of
          "+" -> Just (v1 + v2)
          "-" -> Just (v1 - v2)
          "*" -> Just (v1 * v2)
          "div" ->
              if v2 /= 0 then
                  Just (v1 // v2)
              else
                  Nothing
          _ ->
              Nothing



transitions : Conf -> List Conf
transitions conf
    = conf :: case transition conf of
                  Nothing ->
                      []
                  Just conf1 ->
                      transitions conf1



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
             (Match (ConsP ":" [VarP "h", VarP "t"])
                      (Return (Var "t") "tail"))
       e = App tl (ListLit [Number 1, Number 2, Number 3])
   in (Dict.empty, E e, [])

example4 : Conf
example4 = (Dict.fromList [],M (Match (VarP "h") (Arg (ListLit []) (Match (VarP "t") (Return (Var "t") "tail")))) [Number 1],[MatchEnd])


example5 : Conf
example5 =
    let
        heap = Dict.singleton "sum"
               (Lam (Just "sum")
                    (Alt (Match (ConsP ":" [VarP "h",VarP "t"])
                              (Return (InfixOp "+"
                                           (Var "h")
                                           (App (Var "sum") (Var "t")))
                                           "sum-1"))
                         (Match (ListP [])
                              (Return (Number 1) "sum-2"))))
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
                              (Return (Number 1) "fact-1"))
                         (Match (BangP "n")
                              (Return (InfixOp "*" (Var "n") (App (Var "fact") (InfixOp "-" (Var "n") (Number 1)))) "sum-2"))))
        control = E (App (Var "fact") (Number 5))
        stack = []
    in
        (heap, control, stack)
           
         
