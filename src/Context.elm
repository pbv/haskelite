{-
  Evaluation contexts for evaluating to full normal forms
  Pedro Vasconcelos, 2021-23
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
cons : Tag -> Int -> ExprCtx
cons tag i =
    { getOption = \e -> case e of
                            Cons tag1 args ->
                                if tag==tag1 then
                                    .getOption (Monocle.list i) args
                                else
                                    Nothing
                            _ -> Nothing
    , set = \n e -> case e of
                        Cons tag1 args ->
                            if tag==tag1 then
                                Cons tag1 (.set (Monocle.list i) n args)
                            else
                                e
                        _ -> e
    }
          
