{-
  Parser for Haskelite, a small subset of Haskell
  Pedro Vasconcelos, 2021
-}
module HsParser exposing (..)

import Parser exposing
    (Parser, (|.), (|=), symbol, keyword, variable, 
     succeed, problem, oneOf, andThen, backtrackable, lazy)
import AST exposing (Expr(..), Pattern(..), Decl(..), Name)
import Pretty
import Char
import Set
import List.Extra as List

-- declarations
declListEnd : Parser (List Decl)
declListEnd
    = succeed identity
        |= declList
        |. Parser.end


declList : Parser (List Decl)
declList
    = Parser.sequence
      { start = ""
      , end = ""
      , separator = ""
      , spaces = whitespace
      , item = declaration
      , trailing = Parser.Mandatory
      }

         
declaration : Parser Decl
declaration
    = oneOf
      [ backtrackable typeSignature
      , backtrackable infixEquation
      , prefixEquation
      , lineComment
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

-- comment until end of a line
lineComment : Parser Decl
lineComment = succeed Comment
                |. operator "--"
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
    = Parser.chompWhile Pretty.operatorChar
    |> Parser.getChompedString
    |> andThen (\s -> if String.isEmpty s
                      then problem "operator"
                      else succeed s)

integer : Parser Int
integer = Parser.chompWhile Char.isDigit
          |> Parser.getChompedString
          |> andThen (\s -> case String.toInt s of
                                Nothing -> problem "integer"
                                Just n -> succeed n)

        
-- patterns
pattern =
    oneOf
    [ succeed VarP
           |= identifier
    , succeed BangP
           |. operator "!"
           |= identifier   
    , succeed (BooleanP True)
           |. keyword "True"
    , succeed (BooleanP False)
           |. keyword "False"
    , succeed NumberP
           |= integer -- backtrackable int
    , succeed ListP
         |= Parser.sequence
            { start = "["
            , end = "]"
            , separator = ","
            , spaces = spaces
            , item = lazy (\_ -> pattern)
            , trailing = Parser.Forbidden
            }
    , backtrackable
          <| succeed makeTupleP
               |= Parser.sequence
                  { start = "("
                  , end = ")"
                  , separator = ","
                  , spaces = spaces
                  , item = lazy (\_ -> pattern)
                  , trailing = Parser.Forbidden
                  }
    , Parser.sequence
          { start = "("
          , end = ")"
          , separator = ":"
          , spaces = spaces
          , item = lazy (\_ -> pattern)
          , trailing = Parser.Forbidden
          } |> andThen
                 (\l -> case List.reverse l of
                            [] -> problem "pattern"
                            (p::ps) -> succeed (List.foldr ConsP p
                                                    (List.reverse ps)))
    ]


makeTupleP l =
    case l of
        [x] -> x
        _ -> TupleP l

    
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
topExprEnd
    = succeed identity
        |= topExpr
        |. Parser.end

  
topExpr : Parser Expr
topExpr = infix2

infix7 = infixLeft  applicativeExpr [ ("*", InfixOp "*") ]
infix6 = infixLeft  infix7  [ ("+", InfixOp "+")
                            , ("-", InfixOp "-") ]
infix5 = infixRight infix6 [ (":", Cons)
                           , ("++", InfixOp "++")
                           ]
infix4 = infixLeft  infix5 [ ("==", InfixOp "==")
                           , ("/=", InfixOp "/=")
                           , ("<=", InfixOp "<=")
                           , (">=", InfixOp ">=")
                           , ("<", InfixOp "<")
                           , (">", InfixOp ">")
                           ] -- TODO: these should be non-associative

infix3 = infixRight infix4 [ ("&&", InfixOp "&&") ]
infix2 = infixRight infix3 [ ("||", InfixOp "||") ]


-- parse a given operator
-- maximal munch of operator chars 
operator : String -> Parser ()
operator s
    = backtrackable 
      (Parser.chompWhile Pretty.operatorChar
      |> Parser.getChompedString
      |> andThen (\r -> if s==r
                        then succeed ()
                        else problem ("operator " ++ s)))


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
applicativeExpr
    = oneOf [ if_then_else, lambda, prefixNeg, backtrackable infixApp, application ]


      

-- self-delimited expressions
--
delimited : Parser Expr
delimited =
    oneOf
    [ succeed Var
          |= identifier
    , succeed Number
          |= integer
    , succeed (Boolean True)
          |. keyword "True"
    , succeed (Boolean False)
          |. keyword "False"
    , backtrackable <|
        succeed Var
          |. symbol "("
          |= infixOperator
          |. symbol ")"
    , backtrackable <|
        succeed (\e1 -> App (Var "enumFrom") [e1])
            |. symbol "["
            |= lazy (\_ -> topExpr)
            |. symbol ".."
            |. spaces
            |. symbol "]"
    , backtrackable <|
        succeed (\e1 e2 -> App (Var "enumFromThen") [e1,e2])
            |. symbol "["
            |= lazy (\_ -> topExpr)
            |. symbol ","
            |. spaces
            |= lazy (\_ -> topExpr)
            |. symbol ".."
            |. spaces
            |. symbol "]"
    , backtrackable <|
        succeed (\e1 e2 -> App (Var "enumFromTo") [e1,e2])
            |. symbol "["
            |= lazy (\_ -> topExpr)
            |. symbol ".."
            |. spaces
            |= lazy (\_ -> topExpr)
            |. symbol "]"
    , backtrackable <|
        succeed (\e1 e2 e3 -> App (Var "enumFromThenTo") [e1,e2,e3])
            |. symbol "["
            |= lazy (\_ -> topExpr)
            |. symbol ","
            |. spaces
            |= lazy (\_ -> topExpr)
            |. symbol ".."
            |. spaces
            |= lazy (\_ -> topExpr)
            |. symbol "]"

    , literalTuple
    , literalList
    ]

literalList : Parser Expr
literalList
    = succeed ListLit
         |= Parser.sequence
            { start = "["
            , end = "]"
            , separator = ","
            , spaces = spaces
            , item = lazy (\_ -> topExpr)
            , trailing = Parser.Forbidden
            }

literalTuple : Parser Expr
literalTuple
    = succeed makeTuple
         |= Parser.sequence
            { start = "("
            , end = ")"
            , separator = ","
            , spaces = spaces
            , item = lazy (\_ -> topExpr)
            , trailing = Parser.Forbidden
            }

makeTuple l =
    case l of
        [x] -> x
        _ -> TupleLit l

makeApp e0 args =
    case args of
        [] -> e0
        _ -> App e0 args


prefixNeg : Parser Expr
prefixNeg
    = succeed (\e -> App (Var "negate") [e])
         |. symbol "-"
         |= delimited
            
             
infixApp : Parser Expr
infixApp
    = succeed (\e1 f e2 -> InfixOp f e1 e2)
         |= delimited
         |. spaces   
         |. symbol "`"
         |= identifier
         |. symbol "`"
         |. spaces   
         |= delimited   
             
application : Parser Expr       
application
    = succeed makeApp
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

whitespace
    = Parser.chompWhile (\c -> c==' ' || c=='\n' || c=='\r')
