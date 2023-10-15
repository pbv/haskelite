module Tests exposing (..)

import AST exposing (Expr(..), Matching(..), Pattern(..))
import Machine exposing (Conf, Control(..))
import Heap

--------------------------------------------------------------------
-- examples for debugging 
-------------------------------------------------------------------

-- debugging function
transitions : Conf -> ()
transitions conf = transitions_ 0 conf
              
transitions_ : Int -> Conf -> ()
transitions_ n conf
    = let
        _ = Debug.log (String.fromInt n) (Machine.getControl conf, Machine.getStack conf )
      in
       case Machine.transition conf of
           Nothing ->
               ()
           Just conf1 ->
               transitions_ (n+1) conf1


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


example11
    = (Heap.empty,
           E (Case (InfixOp "==" (Number 1) (Number 2))
                  [(ConsP "True" [], Number 42)]),
           
                      [])


{-
-- extra debugging stuff                 
observe : a -> b -> b
observe x y
    = let
        _ = Debug.log ">>>" x
      in  y
-}

