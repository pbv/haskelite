{-
  Expression evaluation contexts;
  these are used for forcing evaluation to weak/full normal form
  Pedro Vasconcelos, 2021-24
-} 
module Context exposing (..)

import AST exposing (Expr(..), Tag)

import Monocle.Common as Monocle
import Monocle.Optional exposing (Optional)

-- evaluation context for expressions
type alias ExprCtx
    = Optional Expr Expr

-- empty context, i.e. the entire expression
empty : ExprCtx
empty = { getOption = \expr -> Just expr
        , set = \new _ -> new
        }

-- context for ith constructor argument
cons : Int -> ExprCtx
cons i =
    { getOption = \e -> case e of
                            Cons _ args ->
                                .getOption (Monocle.list i) args
                            _ -> Nothing
    , set = \n e -> case e of
                        Cons tag args ->
                            Cons tag (.set (Monocle.list i) n args)
                        _ -> e
    }
          

-- contexts for application nodes
appL : ExprCtx
appL =
    { getOption = \e -> case e of
                            App e1 e2 ->
                                Just e1
                            _ ->
                                Nothing
    , set = \n e -> case e of
                        App _ e2 ->
                            App n e2
                        _ -> e
    }

appR : ExprCtx
appR =
    { getOption = \e -> case e of
                            App e1 e2 ->
                                Just e2
                            _ ->
                                Nothing
    , set = \n e -> case e of
                        App e1 _ ->
                            App e1 n
                        _ -> e
    }

    
-- contexts for binary operators
binopL : ExprCtx
binopL =
    { getOption = \e -> case e of
                            BinaryOp _ e1 e2 ->
                                Just e1
                            _ ->
                                Nothing
    , set = \n e -> case e of
                        BinaryOp op _ e2 ->
                            BinaryOp op n e2
                        _ ->
                            e
    }

binopR : ExprCtx
binopR =
    { getOption = \e -> case e of
                            BinaryOp _ e1 e2 ->
                                Just e2
                            _ ->
                                Nothing
    , set = \n e -> case e of
                        BinaryOp op e1 _ ->
                            BinaryOp op e1 n
                        _ ->
                            e
    }
    
