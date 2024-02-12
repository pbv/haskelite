{- 
   Default methods for pretty-printing stuff
   Pedro Vasconcelos, 2024
-}
module Shows exposing (..)
import AST exposing (..)
import Types  exposing (..)
import Pretty
import PrettyPrinter
import Machine.Heap as Heap

defaultOpts = { prettyLists = True, prettyEnums = False, layout = False, justifications = False }
      
showExpr : Expr -> String
showExpr
    = PrettyPrinter.prettyExpr defaultOpts Heap.empty

showType : Type -> String
showType t
    = PrettyPrinter.prettyType t

showPattern : Pattern -> String
showPattern p
    = PrettyPrinter.prettyPattern p

           
quote : String -> String
quote name
    = "’" ++ name ++ "’"

      
