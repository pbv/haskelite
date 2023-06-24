{-
  Pretty-printer for Haskelite expressions and typers
  Pedro Vasconcelos 2021-23
-}
module Pretty exposing (..)

import AST exposing (Expr(..), Pattern(..), Decl(..), Info, Name)
import Types exposing (Type(..))

-- pretty printing expressions etc
prettyExpr : Expr -> String
prettyExpr e = prettyExpr_ 0 e

prettyExpr_ : Int ->  Expr -> String
prettyExpr_ prec e =
    case e of
        Number n -> paren (prec>0 && n<0) <| String.fromInt n

        Var x -> if AST.isOperator x then "("++x++")" else x

        ListLit l ->
            "[" ++
            (String.join "," <| List.map prettyExpr l) ++
            "]"
                
        TupleLit l ->
            "(" ++
            (String.join "," <| List.map prettyExpr l) ++
            ")"

        Cons ":" [e1, e2] ->
            paren (prec>0)
                <| prettyExpr_ 1 e1 ++ ":" ++ prettyExpr_ 1 e2

        Cons tag [] ->
            tag
                    
        Cons tag args ->
            paren (prec>0) <|
                tag ++ " " ++
                String.join " " (List.map (prettyExpr_ 1) args)
                    

        App (Var "enumFrom") e1 ->
            "[" ++ prettyExpr_ 1 e1 ++ "..]"

        App (App (Var "enumFromThen") e1) e2 ->
            "[" ++ prettyExpr_ 1 e1 ++ "," ++ prettyExpr_ 1 e2 ++ "..]"
                
        App (App (Var "enumFromTo") e1) e2 ->
            "[" ++ prettyExpr_ 1 e1 ++ ".." ++ prettyExpr_ 1 e2 ++ "]"

        App (App (App (Var "enumFromThenTo") e1) e2) e3 ->
            "[" ++ prettyExpr_ 1 e1 ++ "," ++ prettyExpr_ 1 e2 ++ ".."
                ++ prettyExpr_ 1 e3 ++ "]"

        InfixOp op e1 e2 ->
            paren (prec>0)
                <| prettyExpr_ 1 e1 ++ formatOperator op ++ prettyExpr_ 1 e2 

        App e0 e1 ->
            paren (prec>0) <|
            prettyExpr e0 ++ " " ++ prettyExpr_ 1 e1

        Lam (Just id) m ->
            id

        Lam Nothing _ ->
            "<function>"
                
        IfThenElse e1 e2 e3 ->
            paren (prec>0)
                <|  "if " ++ prettyExpr e1 ++ " then " ++
                    prettyExpr e2 ++ " else " ++ prettyExpr e3

        Error ->
            "<error>"



paren : Bool -> String -> String
paren b str
    = if b then "("++str++")" else str


-- format an infix operator, sometimes with spaces either side
formatOperator : Name -> String
formatOperator op
    = if op=="&&" || op == "||"
      then " " ++ op ++ " "
      else if AST.isOperator op then op else "`" ++ op ++ "`"

      
                    
prettyPattern : Pattern -> String
prettyPattern p =
    case p of
        VarP x ->
            x
        BangP x ->
            "!"++x
        NumberP n ->
            String.fromInt n
        TupleP ps ->
            "(" ++ String.join "," (List.map prettyPattern ps) ++ ")"
        ListP ps ->
            "[" ++ String.join "," (List.map prettyPattern ps) ++ "]"
        ConsP ":" [p1,p2] ->
            "(" ++ prettyPattern p1 ++ ":" ++ prettyPattern p2 ++ ")"
        ConsP tag [] ->
            tag
        ConsP tag ps ->
            "(" ++ tag ++ " " ++
                String.join " " (List.map prettyPattern ps) ++ ")"
                
           


prettyType : Type -> String
prettyType ty = prettyType_ 0 ty

prettyType_ : Int -> Type -> String
prettyType_ prec ty
    = case ty of
          TyInt ->
              "Int"
          TyBool ->
              "Bool"
          TyVar name ->
              name
          TyGen idx ->
              showGenVar idx
          TyList ty1 ->
              "[" ++ prettyType ty1 ++ "]"
          TyTuple ts ->
              "(" ++ String.join "," (List.map prettyType ts) ++ ")"
          TyFun t1 t2 ->
              paren (prec>0) <|
                  prettyType_ 1 t1 ++ "->" ++ prettyType_ 0 t2 

showGenVar : Int -> String
showGenVar n
    = String.fromChar <| Char.fromCode <| Char.toCode 'a' + n

{-
prettyDecl : Decl -> String
prettyDecl decl =
    case decl of
        TypeSig f ty ->
            f ++ " :: " ++ prettyType ty
                
        Equation f ps expr ->
            case ps of
                [p1, p2] -> if isOperator f then
                                prettyInfix f p1 p2 expr
                            else
                                prettyEquation f ps expr
                _ -> prettyEquation f ps expr

prettyEquation f ps expr
    = (String.join " " <| (f :: List.map prettyPattern ps))
      ++ " = " ++ prettyExpr expr

prettyInfix f p1 p2 expr
    = prettyPattern p1 ++ " " ++ f ++ " " ++
      prettyPattern p2 ++ " = " ++ prettyExpr expr


prettyInfo : Info -> String
prettyInfo info =
    case info of
        Prim str -> str
        Rewrite decl -> prettyDecl decl
-}
          
                    
