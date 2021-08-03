
module Pretty exposing (..)

import AST exposing (Expr(..), Pattern(..), Decl(..), Name)
import Parser

import List.Extra as List

-- * pretty print parsing errors
deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString deadEnds
    =
      let
          groups = List.groupWhile (\a b -> a.row==b.row &&
                                            a.col==b.col) deadEnds
      in
          String.join "; " <|
          List.map (\(a,r) ->
                        "line " ++ String.fromInt a.row  ++ "," ++
                        "col " ++ String.fromInt a.col ++ ": " ++
                        "expecting " ++
                        (String.join ", " <|
                             List.map (\d -> problemToString d.problem) (a::r))
                   ) groups
      
problemToString : Parser.Problem -> String
problemToString prob
    = case prob of
          Parser.Expecting s -> s
          Parser.ExpectingInt -> "integer"
          Parser.ExpectingVariable -> "variable"
          Parser.ExpectingSymbol s -> s
          Parser.ExpectingKeyword s -> s
          Parser.ExpectingEnd -> "end of input"
          Parser.Problem s -> s
          _ -> "?"


-- * pretty printing expressions etc
prettyExpr : Expr -> String
prettyExpr e =
    case e of
        Number n -> String.fromInt n

        Boolean b -> if b then "True" else "False"

        Var x -> if isOperator x then "("++x++")" else x

        ListLit l ->
            "[" ++
            (String.join ", " <| List.map prettyExpr l) ++
            "]"
                

        Cons e1 e2 ->
            "(" ++ prettyExpr e1 ++ ":" ++ prettyExpr e2 ++ ")"

        InfixOp op e1 e2 ->
            "(" ++ prettyExpr e1 ++ op ++ prettyExpr e2 ++ ")"

        App e0 args ->
            "(" ++ 
                (String.join " " <| List.map prettyExpr (e0::args)) ++
            ")"

        Lam xs e1 ->
            "(\\" ++ String.join " " xs ++ " -> " ++
                prettyExpr e1 ++ ")"
                
        IfThenElse e1 e2 e3 ->
            "(if " ++ prettyExpr e1 ++ " then " ++
                prettyExpr e2 ++ " else " ++ prettyExpr e3 ++ ")"

        Fail msg -> "!"++msg


prettyPattern : Pattern -> String
prettyPattern p =
    case p of
        VarP x ->
            x
        BooleanP b ->
            if b then "True" else "False"
        NumberP n ->
            String.fromInt n
        NilP ->
            "[]"
        ConsP p1 p2 ->
            "(" ++ prettyPattern p1 ++ ":" ++
                prettyPattern p2 ++ ")"

prettyDecl : Decl -> String
prettyDecl decl =
    case decl of
        TypeSig f str ->
            f ++ " :: " ++ str
                
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


operatorChar : Char -> Bool
operatorChar c =
    c=='+' || c=='*' || c=='-' || c=='>' || c=='<' || c==':' || c=='=' 


isOperator : Name -> Bool
isOperator = String.all operatorChar 
                    
          
