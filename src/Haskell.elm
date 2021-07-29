
module Haskell exposing (..)

import Parser exposing
    (Parser, (|.), (|=), int, symbol, keyword, variable, 
     succeed, problem, oneOf, andThen, backtrackable, lazy, run )
import AST
import Char
import Set



-- declarations
declList : Parser (List AST.Decl)
declList
    = Parser.sequence
      { start = ""
      , end = ""
      , separator = ""
      , spaces = newlines
      , item = declaration
      , trailing = Parser.Mandatory
      }

         
declaration : Parser AST.Decl
declaration
    = oneOf
      [ backtrackable typeSignature
      , backtrackable infixEquation
      , prefixEquation
      ]

prefixEquation : Parser AST.Decl
prefixEquation
    = succeed AST.Equation 
         |= identifier
         |= patternList
         |. spaces   
         |. operator "="
         |. spaces
         |= topExpr

infixEquation : Parser AST.Decl
infixEquation
    = succeed (\p1 fun p2 e -> AST.Equation fun [p1,p2] e)
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
typeSignature : Parser AST.Decl
typeSignature
    = succeed AST.TypeSig
         |= identifierOrOperator
         |. spaces
         |. operator "::"
         |. spaces   
         |= (Parser.chompUntilEndOr "\n"
            |> Parser.getChompedString)


identifierOrOperator : Parser AST.Name
identifierOrOperator
    = oneOf [ identifier
            , succeed identity
                |. symbol "("
                |= infixOperator
                |. symbol ")"
            ]
    
            
infixOperator : Parser String
infixOperator
    = Parser.chompWhile isOperator
    |> Parser.getChompedString
    |> andThen (\s -> if String.length s>0 then succeed s
                      else problem "null operator")

isOperator : Char -> Bool
isOperator c = c=='+' || c=='*' || c=='-' || c=='>' || c=='<' || c==':' || c=='=' 
                   
-- patterns
pattern =
    oneOf
    [ succeed AST.VarP
           |= identifier
    , succeed (AST.BooleanP True)
           |. keyword "True"
    , succeed (AST.BooleanP False)
           |. keyword "False"
    , succeed AST.NumberP
           |= backtrackable int
    , succeed AST.NilP
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
                            (p::ps) -> succeed (List.foldr AST.ConsP p
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
topExpr : Parser AST.Expr
topExpr = infix4

infix7 = infixLeft  applicativeExpr [ ("*", AST.InfixOp "*") ]
infix6 = infixLeft  infix7  [ ("+", AST.InfixOp "+")
                            , ("-", AST.InfixOp "-") ]
infix5 = infixRight infix6 [ (":", AST.Cons)
                           , ("++", AST.InfixOp "++")
                                 -- \x y -> AST.App (AST.App (AST.Var "++") x) y)
                           ]
infix4 = infixLeft  infix5 [ ("==", AST.InfixOp "==")
                           , ("/=", AST.InfixOp "/=")
                           ]
-- TODO: these should be non-associative


-- parse a given operator
-- maximal munch of operator chars 
operator : String -> Parser ()
operator s
    = backtrackable 
      (Parser.chompWhile isOperator
      |> Parser.getChompedString
      |> andThen (\r -> if s==r then succeed ()
                        else problem ("expecting operator " ++ s ++ ", got "++r)))


type alias Operator = AST.Expr -> AST.Expr -> AST.Expr
    
-- parse infix left associative operators
--
infixLeft : Parser AST.Expr -> List (AST.Name, Operator) -> Parser AST.Expr
infixLeft operand table
    = succeed identity
         |= operand
         |. spaces
      |> andThen (infixLeftCont operand table)

infixLeftCont : Parser AST.Expr -> List (AST.Name, Operator) -> AST.Expr -> Parser AST.Expr     
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
infixRight : Parser AST.Expr -> List (AST.Name, Operator) -> Parser AST.Expr
infixRight operand table
    = succeed identity
         |= operand
         |. spaces
      |> andThen (infixRightCont operand table)

infixRightCont : Parser AST.Expr -> List (AST.Name, Operator) -> AST.Expr
               -> Parser AST.Expr     
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
              


applicativeExpr : Parser AST.Expr    
applicativeExpr = oneOf [ if_then_else, lambda, application ]


-- self-delimited expressions
--
delimited : Parser AST.Expr
delimited =
    oneOf
    [ succeed AST.Var
          |= identifier
    , succeed AST.Number
          |= backtrackable int -- BUG: int should only consume input if it succeeds
    , succeed (AST.Boolean True)
          |. keyword "True"
    , succeed (AST.Boolean False)
          |. keyword "False"
    , succeed identity
          |. symbol "("
          |= lazy (\_ -> topExpr)
          |. symbol ")"
    , Parser.map AST.ListLit literalList
    ]

literalList : Parser (List AST.Expr)
literalList
    =  Parser.sequence
       { start = "["
       , end = "]"
       , separator = ","
       , spaces = spaces
       , item = lazy (\_ -> topExpr)
       , trailing = Parser.Forbidden
       }

       
application : Parser AST.Expr       
application
    = succeed (\fun args -> List.foldl (\x y -> AST.App y x) fun args)
         |= delimited
         |. spaces
         |= delimitedList

delimitedList : Parser (List AST.Expr)
delimitedList
    = Parser.sequence
      { start = ""
      , end = ""
      , separator = ""
      , spaces = spaces
      , item = delimited
      , trailing = Parser.Forbidden
      }
           

lambda : Parser AST.Expr                      
lambda
    = succeed (\xs e -> List.foldr AST.Lam e xs)
         |. symbol "\\"
         |. spaces
         |= identifierList
         |. spaces
         |. symbol "->"
         |. spaces
         |= lazy (\_ -> topExpr)

if_then_else : Parser AST.Expr                
if_then_else
    = succeed AST.IfThenElse
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

