{- 
   Default methods for pretty-printing stuff
   Pedro Vasconcelos, 2024
-}
module Shows exposing (..)
import AST exposing (..)
import Types  exposing (..)
import Pretty
import Machine.Heap as Heap

showExpr : Expr -> String
showExpr = Pretty.prettyExpr Pretty.defaultOpts Heap.empty

showType : Type -> String
showType = Pretty.prettyType

showVar : Name -> String
showVar name
    = "`" ++ name ++ "'"

showCons : Tag -> String
showCons con
    = "`" ++ con ++ "'"
      
