{- 
   Default functoins for pretty-printing stuff to strings
   Pedro Vasconcelos, 2024
-}
module Shows exposing (..)

import AST exposing (Expr, Pattern)
import Types exposing (Type)
import Pretty
import HsPretty 

-- very high number of columns for pretty-printing without layout
infiniteLength : Int
infiniteLength
    = 1000000

showType : Type -> String
showType ty
    = Pretty.pretty infiniteLength (HsPretty.ppType 0 ty)
      
showExpr : Expr -> String
showExpr e
    = let
        ctx = { prettyLists = True
              , prettyEnums = True
              , line = Pretty.space
              , softline = Pretty.space
              }
      in Pretty.pretty infiniteLength (HsPretty.ppExpr ctx 0 0 e)

showPattern : Pattern -> String
showPattern p
    = Pretty.pretty infiniteLength (HsPretty.ppPattern p)

quote : String -> String
quote name
    = "’" ++ name ++ "’"

      
