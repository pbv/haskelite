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
import Parser.Workaround 
import Indent 
import AST exposing (Expr(..),
                     Matching(..),
                     Pattern(..),
                     Decl(..), DataDecl, Bind, Module, Program(..),
                     Name, Info)
import Types exposing (Type(..), Tycon, Tyvar,
                           tyInt, tyBool, tyChar, tyConst)
import Char
import Set exposing (Set)
import Dict exposing (Dict)
import List.Extra as List

parseProgram : String -> String -> Result String Program
parseProgram inputExpr inputDecls
    = Result.mapError deadEndsToString <|
        (Parser.run topExprEnd inputExpr |>
          Result.andThen (\expr ->
                            Parser.run toplevelModule inputDecls |>
                            Result.andThen (\mod -> Ok (LetProg mod expr))))

----------------------------------------------------------------
-- modules and declarations
----------------------------------------------------------------

toplevelModule : Parser Module
toplevelModule
    = succeed (collectDeclarations recordNames)
                 |= topDeclList
                 |. Parser.end
      |> andThen (\mod ->
                      case checkBinds mod.binds of
                          Err msg ->
                              problem msg
                          Ok _ ->
                              succeed mod)


-- check consistent number arguments in bindings
checkBinds : List Bind -> Result String ()
checkBinds binds 
    = case binds of
          [] ->
              Ok ()
          (first::rest) ->
              checkBind first |>
              Result.andThen (\_ -> checkBinds rest)

checkBind : Bind -> Result String ()
checkBind bind
    = case bind.expr of
          Lam _ _ m ->
              case AST.matchingWellformed m of
                  Just _ ->
                      Ok ()
                  Nothing ->
                      Err ("equations for " ++ bind.name ++
                               " to have equal number of arguments")
          _ ->
              Ok ()
          
-- should we record names of bindings?
type alias Naming
    = Name -> Maybe Name

recordNames : Naming
recordNames
    = Just

ignoreNames : Naming
ignoreNames
    = always Nothing
              
-- collect toplevel declarations by identifier and make single bindings
collectDeclarations : Naming -> List Decl -> Module
collectDeclarations naming decls
    = let ddecls = List.filterMap checkData decls
          binds = collectBinds naming decls
      in { dataDecls = ddecls, binds = binds }


collectBinds : Naming -> List Decl -> List Bind
collectBinds naming decls
    = List.filterMap (makeBind naming) <|
      List.groupWhile (\d1 d2 -> AST.declName d1==AST.declName d2) decls


              
checkData : Decl -> Maybe DataDecl
checkData decl
    = case decl of
          Data d -> Just d
          _ -> Nothing
          
makeBind : Naming -> (Decl, List Decl) -> Maybe Bind
makeBind naming pair =
    case pair of
        (TypeSig id ty, rest) ->
            let m = collectAlts rest
            in 
            Just { name = id,
                   typeSig = Just ty,
                   expr = AST.lambda (naming id) m
                 }
        (Equation id match, rest) ->
            let m = collectAlts (Equation id match::rest)
            in 
            Just { name = id,
                   typeSig = Nothing,
                   expr = AST.lambda (naming id) m
                 }
        _ ->
            Nothing
            

collectAlts : List Decl -> Matching
collectAlts = List.foldr joinDecl Fail

joinDecl : Decl -> Matching -> Matching    
joinDecl decl match1
    = case decl of
          Equation _ match2 ->
              joinAlt match2 match1
          _ ->
              match1

             
-- toplevel declarations
topDeclList : Parser (List Decl)
topDeclList
    = Parser.sequence
      { start = ""
      , end = ""
      , separator = ""
      , spaces = whitespaceOrComment
      , item = topDeclaration
      , trailing = Parser.Forbidden
      }

-- indented local declarations
indentedDeclList : Parser (List Decl)
indentedDeclList
    = Indent.list declaration "indented binding"
      |> andThen (\decls -> if List.isEmpty decls then
                                problem "non-empty bindings"
                            else
                                succeed decls)

        
-- a single top-level declaration
topDeclaration : Parser Decl
topDeclaration
    = oneOf
      [ dataDeclaration
      , typeDeclaration
      , backtrackable typeSignature
      , backtrackable infixEquation 
      , prefixEquation
      ]


-- local declarations; type declarations not allowed
declaration : Parser Decl
declaration
    = oneOf
      [ backtrackable typeSignature
      , backtrackable infixEquation 
      , prefixEquation
      ]

----------------------------------------------------    
-- type alias declarations
----------------------------------------------------
typeDeclaration : Parser Decl
typeDeclaration
    = succeed (\(tycon,vs) ty -> TypeAlias {tycon=tycon, args=vs, tyexp=ty})
         |. keyword "type"
         |. spaces
         |= simpleType
         |. spaces
         |. operator "="
         |. spaces
         |= typeExpr
    
------------------------------------------------------------    
-- data type declarations
------------------------------------------------------------
dataDeclaration : Parser Decl
dataDeclaration
    = succeed makeDataDecl
         |. keyword "data"
         |. spaces
         |= simpleType
         |. spaces
         |. operator "="
         |. spaces
         |= dataAlternatives

makeDataDecl : (Tycon, List Tyvar) -> List (Name, List Type) -> Decl
makeDataDecl (tycon, vs) alts
    = let
        tyresult = TyConst tycon (List.map TyVar vs)
        tyalts = List.map (\(con, tyargs) -> (con, makeArrows tyresult tyargs)) alts
      in
        Data { tycon=tycon, args=vs, alternatives=tyalts }

makeArrows : Type -> List Type -> Type
makeArrows tyresult ts
    = List.foldr TyFun tyresult ts
            

-- a simple type for declarations and aliases
-- i.e. T v1 v2 .. vn
-- where T is a type constructor and vs are distinct type variables
simpleType : Parser (Tycon, List Tyvar)
simpleType
    = succeed Tuple.pair
         |= upperIdentifier
         |. spaces
         |= (identifiers |>
                 andThen (ensure List.allDifferent "distinct type variables"))

              
-- a sequence of identifiers separated by spaces
identifiers : Parser (List Name)
identifiers =
    Parser.sequence
    { start = ""
    , end = ""
    , separator = ""
    , spaces = spaces
    , item = identifier
    , trailing = Parser.Forbidden
    }

            
       
dataAlternatives : Parser (List (Name, List Type))
dataAlternatives
    = Parser.sequence
      { start = ""
      , end = ""
      , separator = "|"
      , spaces = whitespace
      , item = dataAlternative
      , trailing = Parser.Forbidden               
      }

dataAlternative : Parser (Name, List Type)
dataAlternative
    = succeed (\tag ts -> (tag,ts))
          |= upperIdentifier
          |. spaces
          |= Parser.sequence
             { start = ""
             , end = ""
             , separator = ""
             , spaces = spaces
             , item = delimitedType
             , trailing = Parser.Forbidden                      
             }

            
---------------------------------------------------------------------
prefixEquation : Parser Decl
prefixEquation
    = getParseChomped_ equationLHS |>
      andThen (\((name,patts), prefix) ->
          succeed (makeEquation name patts)
              |= equationAlts prefix
              |. whitespace
              |= whereBindings)

-- helper function to construct an equation 
makeEquation : Name -> List Pattern -> List Matching -> List Bind -> Decl
makeEquation name patts alts binds
    = Equation name (makePatterns patts (makeBindings binds (joinAlts alts)))
                      
whereBindings : Parser (List Bind)
whereBindings
    = oneOf
      [ succeed identity
          |. keyword "where"
          |= lazy (\_ -> bindings)
      , succeed []
      ]
                 

infixEquation : Parser Decl
infixEquation
    = getParseChomped infixEquationAux
          

infixEquationAux : Parser (String -> Decl)
infixEquationAux                   
    = succeed (\p1 id p2 e info ->
                   Equation id (Match p1 (Match p2 (Return e (Just info)))))
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
         |= (patternSeq "" "" "" 
            |> andThen
               (ensure distinctPatterns "distinct pattern variables"))


-- equation alternatives
-- i.e. list of guards and expressions or a single return expression
equationAlts : String -> Parser (List Matching)
equationAlts prefix
    = oneOf
      [ Indent.list (equationGuard prefix) "indented guard"
             |> andThen (ensure (List.isEmpty >> not) "alternative")
      , succeed (\(expr,info) -> [Return expr (Just (prefix++info))])
           |= getParseChomped_ equationRHS
      ]
  
equationRHS : Parser Expr
equationRHS
    = succeed identity
           |. spaces
           |. operator "="
           |. spaces
           |= topExpr   


-- an equation guard,
-- i.e. "| cond = expr"
equationGuard :  String -> Parser Matching
equationGuard prefix 
    = succeed (\ ((e1,e2), info) -> makeGuard e1 e2 (prefix++info))
         |= getParseChomped_ guardedExpr
              
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


makeBindings : List Bind -> Matching -> Matching
makeBindings binds match
    = case binds of
          [] ->
              match
          _ ->
              Where binds match

makeGuard : Expr -> Expr -> String -> Matching
makeGuard guard expr info
    = case guard of
          -- shortcircuit redundant conditions
          Var "otherwise" ->
              Return expr (Just info)
          Cons "True" [] ->
              Return expr (Just info)
          Cons "False" [] ->
              Fail
          _ ->
              Arg guard (Match (ConsP "True" []) (Return expr (Just info)))
              

-- join many matchings into an alternative
joinAlts : List Matching -> Matching
joinAlts = List.foldr joinAlt Fail

-- join two matchings           
-- eliminating redudant Fails 
joinAlt : Matching -> Matching -> Matching
joinAlt m1 m2
    = case m1 of
          Fail -> m2
          _ -> case m2 of
                   Fail -> m1
                   _ -> Alt m1 m2
             
             
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

-- principal non-terminal for type expressions
typeExpr : Parser Type
typeExpr
    = succeed insertArrows
           |= typeApplication
           |. spaces
           |= oneOf
              [ Parser.sequence
                { start = "->"
                , end = ""
                , separator = "->"
                , item = typeApplication
                , spaces = spaces
                , trailing = Parser.Forbidden
                }
              , succeed []
              ]
                    

-- insert arrows between a non-empty sequence of types
-- (t0::ts)
insertArrows : Type -> List Type -> Type
insertArrows t0 ts
    = case List.unconsLast ts of
          Nothing -> t0
          Just (t1, ts1) -> List.foldr TyFun t1 (t0::ts1)

              
typeApplication : Parser Type
typeApplication
    = oneOf
      [succeed tyConst
          |= upperIdentifier
          |. spaces
          |= Parser.sequence
             { start = ""
             , end = ""
             , separator = ""
             , item = delimitedType
             , spaces = spaces
             , trailing = Parser.Forbidden
             }
      , delimitedType
      ]

           
          
-- self-delimited types
delimitedType : Parser Type
delimitedType
    = oneOf
      [ succeed (\c -> tyConst c [])
            |= upperIdentifier
      , succeed TyVar        -- identifiers are parsed as free type vars
            |= identifier    -- should be generalized if necessary
      , succeed TyList
            |. symbol "["
            |= lazy (\_ -> typeExpr)
            |. symbol "]"
      , backtrackable
            <| succeed tyTuple
                  |= Parser.sequence
                     { start = "("
                     , end = ")"
                     , separator = ","
                     , item = lazy (\_ -> typeExpr)
                     , spaces = spaces
                     , trailing = Parser.Forbidden
                     }
      , succeed TyConst
             |. symbol "("
             |= upperIdentifier
             |. spaces   
             |= Parser.sequence
                { start = ""
                , end = ""
                , separator = ""
                , spaces = spaces
                , item = lazy (\_ -> typeExpr)
                , trailing = Parser.Forbidden
                }
      ]


          
tyTuple : List Type -> Type
tyTuple ts
    = case ts of
          [t] -> t
          _ -> TyTuple ts
              

            
               
------------------------------------------------------------------
-- auxiliary parsers for expresssions
-----------------------------------------------------------------
               
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


             
-- delimited patterns
pattern : Parser Pattern
pattern =
    oneOf
    [ succeed (\id -> if id == "_" then DefaultP else VarP id)
           |= identifier
    , succeed BangP
           |. symbol "!"
           |= identifier
    , succeed (\c -> ConsP c [])
           |= upperIdentifier
    , succeed NumberP
           |= integer
    , succeed CharP
           |= charLiteral
    , succeed stringPattern
           |= stringLiteral
    , succeed AST.listPattern
         |= patternSeq "[" "]" "," 
    , backtrackable
          <| succeed AST.tuplePattern
               |= patternSeq "(" ")" "," 
    , backtrackable
          <| succeed consPattern
               |= patternSeq "(" ")" ":"
    , succeed ConsP
            |. symbol "("
            |= upperIdentifier
            |. spaces
            |= patternSeq "" "" ""
            |. symbol ")"   
    ]

-- non-delimited pattern   
pattern_ : Parser Pattern
pattern_ = 
    oneOf
    [ succeed ConsP
        |= upperIdentifier
        |. spaces
        |= patternSeq "" "" ""
    , pattern
    ]

    
consPattern : List Pattern -> Pattern
consPattern l =
    case List.unconsLast l of
        Nothing ->
            AST.tuplePattern []
        Just (p,ps) ->
            (List.foldr (\x y -> ConsP ":" [x,y]) p ps)

stringPattern : String -> Pattern
stringPattern 
    = String.foldr (\x y -> ConsP ":" [CharP x,y]) (ConsP "[]" []) 

                
-- parse a sequence of patterns with a given start, end and separator
patternSeq : String -> String -> String -> Parser (List Pattern)    
patternSeq open close sep =
    Parser.sequence
    { start = open
    , end = close
    , separator = sep
    , spaces = spaces
    , item = lazy (\_ -> pattern)
    , trailing = Parser.Forbidden
    }

-- * check all pattern variables are distinct
distinctPatterns : List Pattern -> Bool
distinctPatterns ps
    = List.allDifferent <| List.concatMap AST.patternVars ps

    
-- top-level expressions
topExprEnd : Parser Expr
topExprEnd
    = succeed identity
        |= topExpr
        |. Parser.end

  
topExpr : Parser Expr
topExpr = infix2

infix7 = infixLeft  applicativeExpr [ ("*", BinaryOp "*") ]
infix6 = infixLeft  infix7  [ ("+", BinaryOp "+")
                            , ("-", BinaryOp "-") ]
infix5 = infixRight infix6 [ (":", \e1 e2 -> Cons ":" [e1,e2])
                           , ("++", \e1 e2 -> App (App (Var "++") e1) e2)
                           ]
infix4 = infixLeft  infix5 [ ("==", BinaryOp "==")
                           , ("/=", BinaryOp "/=")
                           , ("<=", BinaryOp "<=")
                           , (">=", BinaryOp ">=")
                           , ("<", BinaryOp "<")
                           , (">", BinaryOp ">")
                           ] -- TODO: these should be non-associative

infix3 = infixRight infix4 [ ("&&", \e1 e2 -> App (App (Var "&&") e1) e2) ]
infix2 = infixRight infix3 [ ("||", \e1 e2 -> App (App (Var "||") e2) e2) ]


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
                  letExpr,
                  caseExpr,
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
    , succeed Char
          |= charLiteral
    , succeed stringToList
          |= stringLiteral
    , succeed (\tag -> Cons tag [])
          |= upperIdentifier
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
    = succeed AST.listLit
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
    = succeed AST.tupleLit
         |= Parser.sequence
            { start = "("
            , end = ")"
            , separator = ","
            , spaces = spaces
            , item = lazy (\_ -> topExpr)
            , trailing = Parser.Forbidden
            }

-- build an application to a list of arguments
makeApp : Expr -> List Expr -> Expr
makeApp e0 args
    = case (e0, args) of
          (Var "error", [msg]) ->
              Error msg
          (Cons tag args1, args2) ->
              Cons tag (args1 ++ args2)
          _ ->
              makeApp_ e0 args
                  
-- worker function           
makeApp_ : Expr -> List Expr -> Expr             
makeApp_ e0 args =
    case args of
        [] -> e0
        (e1::rest) -> makeApp_ (App e0 e1) rest


prefixNeg : Parser Expr
prefixNeg
    = succeed (App (Var "negate"))
         |. symbol "-"
         |= delimited
            
             
infixApp : Parser Expr
infixApp
    = succeed (\e1 f e2 -> BinaryOp f e1 e2)
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
                   AST.lambda Nothing
                       (makePatterns patts (Return expr (Just info))))
         |. symbol "\\"
         |. spaces
         |= patternSeq "" "" ""
         |. symbol "->"   
         |. spaces
         |= lazy (\_ -> topExpr)

-- add a list of patterns to a matching 
makePatterns : List Pattern -> Matching -> Matching
makePatterns ps end
    = List.foldr Match end ps


letExpr : Parser Expr
letExpr
    = succeed Let
         |. keyword "let"
         -- |. spaces
         |= lazy (\_ -> bindings)
         |. spaces
         |. keyword "in"
         |. spaces
         |= lazy (\_ -> topExpr)  


caseExpr : Parser Expr
caseExpr
    = succeed Case
         |. keyword "case"
         |. spaces
         |= lazy (\_ -> topExpr)
         |. spaces
         |. keyword "of"
         -- |. whitespace
         |= caseAlts

caseAlts : Parser (List (Pattern,Expr))
caseAlts
    = Indent.list caseAlt "indented case alternative"
      |> andThen (\alts -> if List.isEmpty alts then
                               problem "non-empty case alternatives"
                           else
                               succeed alts)
           

caseAlt : Parser (Pattern, Expr)
caseAlt
    = succeed Tuple.pair
         |= pattern_
         |. spaces   
         |. symbol "->"
         |. spaces
         |= lazy (\_ -> topExpr)
            
    
-- a sequence of indented bindings
bindings : Parser (List Bind)
bindings
    = succeed (collectBinds ignoreNames)
         |= indentedDeclList 
      |> andThen (\binds -> case checkBinds binds of
                                Err msg ->
                                    problem msg
                                Ok _ ->
                                    succeed binds)

            
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
                   

-- lower case identifiers    
identifier : Parser String
identifier
    = variable
      { start = \c -> Char.isLower c || c == '_'
      , inner = \c -> Char.isAlphaNum c || c=='_' || c == '\''
      , reserved = reservedWords
      }

-- upper case identifiers    
upperIdentifier : Parser String
upperIdentifier
    = variable
      { start = \c -> Char.isUpper c
      , inner = \c -> Char.isAlphaNum c || c=='_' || c=='\''
      , reserved = Set.empty
      }

charLiteral : Parser Char
charLiteral 
    = succeed identity
        |. Parser.chompIf (\c -> c == '\'')
        |= character '\''
        |. Parser.chompIf (\c -> c == '\'')
         
stringLiteral : Parser String
stringLiteral 
    = succeed String.fromList
         |= Parser.sequence
            { start = "\""
            , separator = ""
            , spaces = succeed ()
            , end = "\""
            , item = character '\"'
            , trailing = Parser.Forbidden
            }
         

character : Char -> Parser Char
character delimiter
    = oneOf [ succeed identity
                  |. Parser.chompIf (\c -> c=='\\')
                  |= escapeChar
            , getChompedChar (Parser.chompIf (\c -> c/=delimiter))
            ]

escapeChar : Parser Char
escapeChar
    = oneOf [ succeed '\n'
                  |. Parser.chompIf (\c -> c == 'n')
            , succeed '\t'
                  |. Parser.chompIf (\c -> c == 't')
            , getChompedChar
                  (Parser.chompIf (\c -> c == '\\' || c=='\'' || c=='\"'))
            , succeed Char.fromCode
                  |= integer
            ]
      
getChompedChar : Parser a -> Parser Char
getChompedChar p
    = Parser.getChompedString p |>
      andThen (\s -> case String.uncons s of
                         Just (c, "") -> succeed c
                         _ -> problem "character literal")
      
                  
reservedWords : Set String    
reservedWords
    = Set.fromList [ "if", "then", "else",
                     "let", "in", "case", "of", "where", "data" ]


-- consume whitespaces (including newlines) or comments
whitespaceOrComment : Parser ()
whitespaceOrComment
    = Parser.loop 0 <| ifProgress <|
          oneOf
          [ Parser.Workaround.lineCommentAfter "--"
          , Parser.multiComment "{-" "-}" Parser.Nestable
          , whitespace
          ]           

{-      
skipAnnotation : Parser (List String)
skipAnnotation
    = succeed String.words
        |. symbol "--SKIP--"
        |= Parser.getChompedString (Parser.Workaround.chompUntilEndOrAfter "\n")    

annotationOrComment : Parser (List String)
annotationOrComment
    = oneOf
      [ skipAnnotation
      , succeed [] |. Parser.Workaround.lineCommentAfter "--"
      , succeed [] |. Parser.multiComment "{-" "-}" Parser.Nestable
      ]
        
skipAnnotations : Parser (List String)
skipAnnotations
    = succeed List.concat 
      |= Parser.sequence
         { start = ""
         , end = ""
         , separator = ""
         , spaces = whitespace
         , item = annotationOrComment
         , trailing = Parser.Forbidden
         }
-}

-- whitespace, including newlines
whitespace : Parser ()
whitespace    
    = Parser.chompWhile (\c -> c==' ' || c=='\t' || c=='\n' || c=='\r')

-- just spaces and tabulations (no newlines)
spaces : Parser ()
spaces
    = Parser.chompWhile (\c -> c==' ' || c=='\t')

----------------------------------------------------------------------      
-- helper functions
----------------------------------------------------------------------

-- convert a string into an AST expression (list of chars)
stringToList : String -> Expr
stringToList s
    = AST.listLit (List.map Char <| String.toList s)
   
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
      


