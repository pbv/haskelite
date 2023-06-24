{-
  Parser for Haskelite, a small subset of Haskell
  Pedro Vasconcelos, 2021-2023
-}
module HsParser exposing (..)

import Parser exposing (Parser,
                        (|.), (|=),
                        symbol, keyword, variable,
                        succeed, problem, oneOf, andThen,
                        backtrackable, lazy)
import AST exposing (Expr(..),
                     Matching(..),
                     Pattern(..),
                     Decl(..),
                     Bind, Program(..),
                     Name, Info)
import Types exposing (Type(..))
import Char
import Set
import Dict exposing (Dict)
import List.Extra as List



{-
parseProg : String -> String -> Result String Program
parseProg inputExpr inputDecls
    = Result.mapError deadEndsToString <|
        (Parser.run topExprEnd inputExpr |>
             Result.andThen (\expr ->
                                 Parser.run declarations inputDecls |>
                                 Result.andThen (\binds ->
                                                     Ok (Letrec binds expr))))




collectAlts : List Decl -> List Alt
collectAlts decls
    = case decls of
          (Equation _ patts expr :: rest) ->
              (patts,expr) :: collectAlts rest
          _ ->
              []   -- NB: this fails silently if there are multiple type signatures
              
                  
       

  

-}            


----------------------------------------------------------------
-- declarations
----------------------------------------------------------------

declarations : Parser (List Bind)
declarations 
    = succeed collectBinds
          |= declList
          |. Parser.end


-- collect declarations by identifier and make single bindings
collectBinds : List Decl -> List Bind
collectBinds decls
    = List.map makeBind <|
      List.groupWhile (\d1 d2 -> AST.declName d1 == AST.declName d2) decls

makeBind : (Decl, List Decl) -> Bind
makeBind pair =
    case pair of
        (TypeSig id ty, rest) ->
            { name = id,
              typeSig = Just (Types.generalize Set.empty ty),
              expr = Lam (Just id) (collectAlts rest)
            }
        (Equation id match, rest) ->
            { name = id,
              typeSig = Nothing,
              expr = Lam (Just id) (collectAlts (Equation id match::rest))
            }

collectAlts : List Decl -> Matching
collectAlts = List.foldr joinAlt Fail

joinAlt : Decl -> Matching -> Matching              
joinAlt decl match2
    = case decl of
          TypeSig _ _ -> match2
          Equation _ match1 -> case match2 of
                                   Fail -> match1
                                   _    -> Alt match1 match2
             
             
declList : Parser (List Decl)
declList
    = Parser.sequence
      { start = ""
      , end = ""
      , separator = ""
      , spaces = whitespaceOrComment
      , item = declaration
      , trailing = Parser.Mandatory
      }



-- single declaration    
declaration : Parser Decl
declaration
    = oneOf
      [ backtrackable typeSignature
      , backtrackable infixEquation 
      , prefixEquation
      ]  



prefixEquation : Parser Decl
prefixEquation
    = getParseChomped_ equationLHS |>
      andThen (\((id,patts),prefix) ->
                   equationAlts |>
                   andThen
                   (\alts ->
                        case alts of
                            [] ->
                                succeed (\(expr,posfix) ->
                                             Equation id (makeSimpleEquation patts expr (prefix++posfix)))
                                   |= getParseChomped_ equationRHS
                            _ ->
                                succeed (Equation id (makeGuardEquation patts prefix alts))))
    

infixEquation : Parser Decl
infixEquation
    = getParseChomped infixEquationAux
          

infixEquationAux : Parser (String -> Decl)
infixEquationAux                   
    = succeed (\p1 id p2 e info ->
                   Equation id (Match p1 (Match p2 (Return e info))))
         |= pattern
         |. spaces
         |= infixOperator
         |. spaces
         |= pattern
         |. spaces
         |. operator "="
         |. spaces
         |= topExpr

              
               
-- left side of an equation,
-- i.e. an identifier and a list of patterns
equationLHS : Parser (Name, List Pattern)
equationLHS
    = succeed Tuple.pair
         |= identifier
         |. spaces
         |= patternList
          

-- alternative
-- i.e. list of guards and expressions (possibly empty)
equationAlts : Parser (List ((Expr,Expr), String))
equationAlts
    = Parser.sequence
      { start = ""
      , end = ""
      , separator = ""
      , spaces = whitespaceOrComment
      , item = getParseChomped_ guardedExpr
      , trailing = Parser.Mandatory
      }

equationRHS : Parser Expr
equationRHS
    = succeed identity
           |. spaces
           |. operator "="
           |. spaces
           |= topExpr   
    
-- a guard and an expression,
-- i.e. "|" cond "=" expr
guardedExpr : Parser (Expr, Expr)    
guardedExpr
    = succeed Tuple.pair
          |. operator "|"
          |. spaces
          |= topExpr
          |. spaces
          |. operator "="
          |. spaces
          |= topExpr


makeSimpleEquation : List Pattern -> Expr -> String -> Matching
makeSimpleEquation patts expr info
    = makeMatching patts (Return expr info)

makeGuardEquation : List Pattern
                  -> String
                  -> List ((Expr,Expr),String)
                  -> Matching      
makeGuardEquation patts prefix alts
    =  makeAlts <|
          List.map (\((guard,expr),posfix) ->
                        makeMatching patts (Arg guard
                                                (Match (ConsP "True" [])
                                                     (Return expr (prefix++posfix))))
                   ) alts


makeAlts : List Matching -> Matching
makeAlts = List.foldr Alt Fail 
             
             
---------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------
            
typeSignature : Parser Decl
typeSignature
    = succeed TypeSig
         |= identifierOrOperator
         |. spaces
         |. operator "::"
         |. spaces
         |= typeExpr



-- * type expressions
typeExpr : Parser Type
typeExpr
    = succeed tyArrows
        |= typeBase
        |. spaces
        |= oneOf [
            Parser.sequence
                { start = "->"
                , end = ""
                , separator = "->"
                , item = typeBase
                , spaces = spaces
                , trailing = Parser.Forbidden
                }
           , succeed []
           ]

          
typeBase : Parser Type
typeBase
    = oneOf
      [ succeed TyInt
            |. keyword "Int"
      , succeed TyBool
            |. keyword "Bool"
      , succeed TyVar        -- identifier are parsed as free type vars
            |= identifier    -- should be generalized if necessary
      , succeed TyList
            |. symbol "["
            |= lazy (\_ -> typeExpr)
            |. symbol "]"
      , succeed tyTuple
              |= Parser.sequence
                 { start = "("
                 , end = ")"
                 , separator = ","
                 , item = lazy (\_ -> typeExpr)
                 , spaces = spaces
                 , trailing = Parser.Forbidden
                 }
      ]


tyArrows : Type -> List Type -> Type
tyArrows t0 ts
    = case List.unconsLast ts of
          Nothing -> t0
          Just (t1, ts1) -> List.foldr TyFun t1 ([t0]++ts1)
          
tyTuple : List Type -> Type
tyTuple ts
    = case ts of
          [t] -> t
          _ -> TyTuple ts
              
           
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
    = Parser.chompWhile AST.operatorChar
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
pattern : Parser Pattern
pattern =
    oneOf
    [ succeed VarP
           |= identifier
    , succeed BangP
           |. operator "!"
           |= identifier   
    , succeed (ConsP "True" [])
           |. keyword "True"
    , succeed (ConsP "False" [])
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
                            (p::ps) ->
                                succeed (List.foldr
                                         (\x y -> ConsP ":" [x,y])
                                         p
                                         (List.reverse ps)))
    ]


makeTupleP l =
    case l of
        [x] -> x
        _ -> TupleP l

patternList : Parser (List Pattern)    
patternList =
    Parser.sequence
    { start = ""
    , end =""
    , separator= ""
    , spaces=spaces
    , item = pattern
    , trailing = Parser.Forbidden
    } |>
    andThen
    (ensure distinctPatterns "all pattern variables to be distinct")


-- * check all pattern variables are distinct
distinctPatterns : List Pattern -> Bool
distinctPatterns ps
    = List.allDifferent <|
      List.filter (\v -> v/="_") <|
      List.concatMap AST.patternVars ps

    
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
infix5 = infixRight infix6 [ (":", \e1 e2 -> Cons ":" [e1,e2])
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
      (Parser.chompWhile AST.operatorChar
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
    = oneOf [ if_then_else,
                  lambdaExpr,
                  prefixNeg,
                  backtrackable infixApp,
                  application ]


      

-- self-delimited expressions
--
delimited : Parser Expr
delimited =
    oneOf
    [ succeed Var
          |= identifier
    , succeed Number
          |= integer
    , succeed (\tag -> Cons tag [])
          |= constructor
    , backtrackable <|
        succeed Var
          |. symbol "("
          |= infixOperator
          |. symbol ")"
    , backtrackable <|
        succeed (App (Var "enumFrom"))
            |. symbol "["
            |= lazy (\_ -> topExpr)
            |. symbol ".."
            |. spaces
            |. symbol "]"
    , backtrackable <|
        succeed (\e1 e2 -> makeApp (Var "enumFromThen") [e1,e2])
            |. symbol "["
            |= lazy (\_ -> topExpr)
            |. symbol ","
            |. spaces
            |= lazy (\_ -> topExpr)
            |. symbol ".."
            |. spaces
            |. symbol "]"
    , backtrackable <|
        succeed (\e1 e2 -> makeApp (Var "enumFromTo") [e1,e2])
            |. symbol "["
            |= lazy (\_ -> topExpr)
            |. symbol ".."
            |. spaces
            |= lazy (\_ -> topExpr)
            |. symbol "]"
    , backtrackable <|
        succeed (\e1 e2 e3 -> makeApp (Var "enumFromThenTo") [e1,e2,e3])
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

makeApp : Expr -> List Expr -> Expr             
makeApp e0 args =
    case args of
        [] -> e0
        (e1::rest) -> makeApp (App e0 e1) rest


prefixNeg : Parser Expr
prefixNeg
    = succeed (App (Var "negate"))
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
           

lambdaExpr : Parser Expr
lambdaExpr
    = getParseChomped lambdaExprAux 
      
lambdaExprAux : Parser (String -> Expr)  
lambdaExprAux
    = succeed (\patts expr info ->
                   Lam Nothing (makeMatching patts (Return expr info)))
         |. symbol "\\"
         |. spaces
         |= patternList
         |. spaces
         |. operator "->"
         |. spaces
         |= lazy (\_ -> topExpr)

makeMatching : List Pattern -> Matching -> Matching
makeMatching ps end
    = List.foldr Match end ps
           
                        
            
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

constructor : Parser String
constructor
    = variable
      { start = \c -> Char.isUpper c
      , inner = \c -> Char.isAlphaNum c || c=='_' || c=='\''
      , reserved = Set.empty
      }
    
reservedWords
    = Set.fromList [ "if", "then", "else", "let", "in", "case", "of", "where" ]




whitespaceOrComment : Parser ()
whitespaceOrComment
    = Parser.loop 0 <| ifProgress <|
          oneOf
          [ Parser.lineComment "--"
          , Parser.multiComment "{-" "-}" Parser.Nestable
          , whitespace
          ]

-- whitespace, including newlines
whitespace : Parser ()
whitespace    
    = Parser.chompWhile (\c -> c==' ' || c=='\t' || c=='\n' || c=='\r')

-- just space (no newlines)
spaces : Parser ()
spaces
    = Parser.chompWhile (\c -> c==' ' || c=='\t')

----------------------------------------------------------------------      
-- helper functions
----------------------------------------------------------------------

-- combine a parser result with the chomped string
getParseChomped : Parser (String -> a) -> Parser a
getParseChomped parser
    = succeed (\start fun end src ->
                   fun (String.slice start end src))
        |= Parser.getOffset
        |= parser
        |= Parser.getOffset
        |= Parser.getSource

getParseChomped_ : Parser a -> Parser (a, String)
getParseChomped_ parser
    = succeed (\start v end src ->
                   (v, String.slice start end src))
        |= Parser.getOffset
        |= parser
        |= Parser.getOffset
        |= Parser.getSource




ifProgress : Parser a -> Int -> Parser (Parser.Step Int ())
ifProgress parser offset =
  succeed identity
    |. parser
    |= Parser.getOffset
    |> Parser.map (\newOffset -> if offset == newOffset then Parser.Done () else Parser.Loop newOffset)
      

-- check a property and abort parsing if it fails       
ensure : (a -> Bool) -> String -> a -> Parser a
ensure pred msg v
    = if pred v then succeed v else problem msg

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
                             Set.toList <|
                             Set.fromList <|
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
      
-----------------------------------------------------


example1 : String
example1 =
    """
len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

zip [] _ = [] 
zip _ [] = [] 
zip (x:xs) (y:ys) = (x,y) : zip xs ys 
"""

example2 =
    """[] ++ ys = ys
(x:xs) ++ ys = x : (xs++ys)
"""
   
example3 =
   """
foo x | x>0 = 42
      | x<0 = 1+x
      | otherwise = 0
foo y = y+1
"""

example4 = "1+2"
   
