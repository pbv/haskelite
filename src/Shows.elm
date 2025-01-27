{- 
   Default methods for pretty-printing stuff
   Pedro Vasconcelos, 2024
-}
module Shows exposing (..)

import AST exposing (Expr, Pattern)
import Types exposing (Type)
import Pretty
import HsPretty 
import Machine.Heap as Heap
  
showExpr : Expr -> String
showExpr
    = HsPretty.showExpr 

showType : Type -> String
showType 
    = HsPretty.showType 

showPattern : Pattern -> String
showPattern 
    = HsPretty.showPattern 
           
quote : String -> String
quote name
    = "’" ++ name ++ "’"

      
