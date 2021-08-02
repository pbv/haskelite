{-
-- Parser and pretty printer for a tiny subset of Haskell
-}
module Haskell exposing (..)

import Parser exposing
    (Parser, (|.), (|=), int, symbol, keyword, variable, 
     succeed, problem, oneOf, andThen, backtrackable, lazy)
import AST exposing (Expr(..), Pattern(..), Decl(..), Name)
import Char
import Set
import List.Extra as List


-- declarations
declList : Parser (List Decl)
declList
    = Parser.sequence
      { start = ""
      , end = ""
      , separator = ""
      , spaces = newlines
      , item = declaration
      , trailing = Parser.Mandatory
      }

         
declaration : Parser Decl
declaration
    = oneOf
      [ backtrackable typeSignature
      , backtrackable infixEquation
      , prefixEquation
      ]

prefixEquation : Parser Decl
prefixEquation
    = succeed Equation 
         |= identifier
         |= patternList
         |. spaces   
         |. operator "="
         |. spaces
         |= topExpr

infixEquation : Parser Decl
infixEquation
    = succeed (\p1 fun p2 e -> Equation fun [p1,p2] e)
         |= pattern
         |. spaces
         |= infixOperator
         |. spaces
         |= pattern
         |. spaces
         |. operator "="
         |. spaces
         |= topExpr


-- types are parsed but ignored
typeSignature : Parser Decl
typeSignature
    = succeed TypeSig
         |= identifierOrOperator
         |. spaces
         |. operator "::"
         |. spaces   
         |= (Parser.chompUntilEndOr "\n"
            |> Parser.getChompedString)


identifierOrOperator : Parser Name
identifierOrOperator
    = oneOf [ identifier
            , succeed identity
                |. symbol "("
                |= infixOperator
                |. symbol ")"
            ]
    
            
infixOperator : Parser String
infixOperator
    = Parser.chompWhile operatorChar
    |> Parser.getChompedString
    |> andThen (\s -> if String.isEmpty s
                      then problem "null operator"
                      else succeed s)

operatorChar : Char -> Bool
operatorChar c =
    c=='+' || c=='*' || c=='-' || c=='>' || c=='<' || c==':' || c=='=' 


isOperator : Name -> Bool
isOperator = String.all operatorChar 

        
-- patterns
pattern =
    oneOf
    [ succeed VarP
           |= identifier
    , succeed (BooleanP True)
           |. keyword "True"
    , succeed (BooleanP False)
           |. keyword "False"
    , succeed NumberP
           |= backtrackable int
    , succeed NilP
           |. symbol "["
           |. spaces
           |. symbol "]"
    , Parser.sequence
          { start = "("
          , end = ")"
          , separator = ":"
          , spaces = spaces
          , item = lazy (\_ -> pattern)
          , trailing = Parser.Forbidden
          } |> andThen
                 (\l -> case List.reverse l of
                            [] -> problem "invalid pattern"
                            (p::ps) -> succeed (List.foldr ConsP p
                                                    (List.reverse ps)))
    ]

patternList =
    Parser.sequence
    { start = ""
    , end =""
    , separator= ""
    , spaces=spaces
    , item = pattern
    , trailing = Parser.Forbidden
    }
    

    
-- top-level expressions
topExprEnd : Parser Expr
topExprEnd = succeed identity
              |= topExpr
              |. Parser.end


topExpr : Parser Expr
topExpr = infix4

infix7 = infixLeft  applicativeExpr [ ("*", InfixOp "*") ]
infix6 = infixLeft  infix7  [ ("+", InfixOp "+")
                            , ("-", InfixOp "-") ]
infix5 = infixRight infix6 [ (":", Cons)
                           , ("++", InfixOp "++")
                           ]
infix4 = infixLeft  infix5 [ ("==", InfixOp "==")
                           , ("/=", InfixOp "/=")
                           ]
-- TODO: these should be non-associative


-- parse a given operator
-- maximal munch of operator chars 
operator : String -> Parser ()
operator s
    = backtrackable 
      (Parser.chompWhile operatorChar
      |> Parser.getChompedString
      |> andThen (\r -> if s==r
                        then succeed ()
                        else problem ("expecting operator " ++ s ++ ", got "++r)))


type alias BinOp = Expr -> Expr -> Expr
    
-- parse infix left associative operators
--
infixLeft : Parser Expr -> List (Name, BinOp) -> Parser Expr
infixLeft operand table
    = succeed identity
         |= operand
         |. spaces
      |> andThen (infixLeftCont operand table)

infixLeftCont : Parser Expr -> List (Name, BinOp) -> Expr -> Parser Expr     
infixLeftCont operand table accum
    = oneOf
      <| List.map (\(op, func) -> 
                       succeed (func accum)
                           |. operator op
                           |. spaces
                           |= operand
                           |. spaces
                           |> andThen (infixLeftCont operand table)) table
          ++ [succeed accum]

-- parse infix right associative operators
--
infixRight : Parser Expr -> List (Name, BinOp) -> Parser Expr
infixRight operand table
    = succeed identity
         |= operand
         |. spaces
      |> andThen (infixRightCont operand table)

infixRightCont : Parser Expr -> List (Name, BinOp) -> Expr -> Parser Expr
infixRightCont operand table x
    = oneOf
      <| List.map (\(op,func) -> 
                       succeed identity
                           |. operator op
                           |. spaces
                           |= operand
                           |. spaces
                           |> andThen (\y -> infixRightCont operand table y 
                           |> andThen (\r -> succeed (func x r)))
                  ) table
          ++ [succeed x]
              


applicativeExpr : Parser Expr
applicativeExpr = oneOf [ if_then_else, lambda, application ]


-- self-delimited expressions
--
delimited : Parser Expr
delimited =
    oneOf
    [ succeed Var
          |= identifier
    , succeed Number
          |= backtrackable int -- BUG: int should only consume input if it succeeds
    , succeed (Boolean True)
          |. keyword "True"
    , succeed (Boolean False)
          |. keyword "False"
    , succeed identity
          |. symbol "("
          |= lazy (\_ -> topExpr)
          |. symbol ")"
    , Parser.map ListLit literalList
    ]

literalList : Parser (List Expr)
literalList
    =  Parser.sequence
       { start = "["
       , end = "]"
       , separator = ","
       , spaces = spaces
       , item = lazy (\_ -> topExpr)
       , trailing = Parser.Forbidden
       }

       
application : Parser Expr       
application
    = succeed (\e0 args -> case args of
                               [] -> e0
                               _ -> App e0 args)
         |= delimited
         |. spaces
         |= delimitedList

delimitedList : Parser (List Expr)
delimitedList
    = Parser.sequence
      { start = ""
      , end = ""
      , separator = ""
      , spaces = spaces
      , item = delimited
      , trailing = Parser.Forbidden
      }
           

lambda : Parser Expr
lambda
    = succeed Lam 
         |. symbol "\\"
         |. spaces
         |= identifierList
         |. spaces
         |. operator "->"
         |. spaces
         |= lazy (\_ -> topExpr)

if_then_else : Parser Expr                
if_then_else
    = succeed IfThenElse
         |. keyword "if"
         |. spaces
         |= lazy (\_ -> topExpr)
         |. spaces
         |. keyword "then"
         |. spaces
         |= lazy (\_ -> topExpr)
         |. spaces
         |. keyword "else"
         |. spaces
         |= lazy (\_ -> topExpr)
                

identifierList : Parser (List String)
identifierList
    = Parser.sequence
      { start = ""
      , separator = ""
      , end = ""
      , spaces = spaces
      , item  = identifier
      , trailing = Parser.Forbidden
      }
                   

identifier : Parser String
identifier
    = variable
      { start = \c -> Char.isLower c || c == '_'
      , inner = \c -> Char.isAlphaNum c || c=='_' || c == '\''
      , reserved = reservedWords
      }

reservedWords
    = Set.fromList [ "if", "then", "else", "let", "in", "case", "of", "where" ]


spaces
    = Parser.chompWhile (\c -> c==' ')

newlines
    = Parser.chompWhile (\c -> c=='\n' || c=='\r')


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

        Var x -> x

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
