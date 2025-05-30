{-
  A parser for a small subset of Haskell
  Pedro Vasconcelos, 2021-2025
-}
module Language exposing (..)

import Parser exposing (Parser, 
                        (|.), (|=),
                        token, symbol, keyword, variable,
                        succeed, problem, oneOf, andThen, 
                        backtrackable, lazy)
import Parser.Workaround 
import AST exposing (..)
import Types exposing (..)
import Shows
import Set exposing (Set)
import Dict exposing (Dict)
import List.Extra as List


notImplemented : String -> String -> Expr
notImplemented msg src =  Unimplemented <| AST.notImplemented msg src

parseModule : String -> Result String Module
parseModule input
    = Result.mapError deadEndsToString (Parser.run toplevelModule input)

parseExpr : String -> Result String Expr
parseExpr input
    = Result.mapError deadEndsToString (Parser.run topExprEnd input)
            
----------------------------------------------------------------
-- modules and declarations
----------------------------------------------------------------

toplevelModule : Parser Module
toplevelModule
    = succeed collectDeclarations
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
                               " must have equal number of arguments")
          _ ->
              Ok ()
          

-- collect toplevel declarations by identifier and make single bindings
collectDeclarations :  List Decl -> Module
collectDeclarations decls
    = let ddecls = List.filterMap checkData decls
          adecls = List.filterMap checkAlias decls
          binds = collectBinds decls
      in { dataDecls = ddecls, aliasDecls = adecls, binds = binds }


collectBinds : List Decl -> List Bind
collectBinds decls
    = List.filterMap makeBind <|
      List.groupWhile (\d1 d2 -> AST.declName d1==AST.declName d2) decls


              
checkData : Decl -> Maybe DataDecl
checkData decl
    = case decl of
          Data d -> Just d
          _ -> Nothing

checkAlias : Decl -> Maybe AliasDecl
checkAlias decl
    = case decl of
          Alias d -> Just d
          _ -> Nothing
          
makeBind : (Decl, List Decl) -> Maybe Bind
makeBind pair =
    case pair of
        (TypeSig id ty, rest) ->
            let m = collectAlts rest
            in 
            Just { name = id,
                   typeSig = Just ty,
                   expr = AST.lambda (Just id) m
                 }
        (Equation id match, rest) ->
            let m = collectAlts (Equation id match::rest)
            in 
            Just { name = id,
                   typeSig = Nothing,
                   expr = AST.lambda (Just id) m
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

-- local declarations
declList : Parser (List Decl)
declList = sepBy1 declaration semicolon
      
-- a single top-level declaration
topDeclaration : Parser Decl
topDeclaration
    = oneOf
      [ dataDeclaration
      , typeDeclaration  
      , backtrackable typeSignature
      , equation
      ]


-- local declarations;
-- type declarations not allowed
-- infix declaration not allowed to avoid ambiguity with patterns!
declaration : Parser Decl
declaration
    = oneOf
      [ backtrackable typeSignature
      , equation
      ]

    
----------------------------------------------------    
-- type synonym declarations
----------------------------------------------------
typeDeclaration : Parser Decl
typeDeclaration
    = succeed (\(tycon,vs) ty -> Alias {tycon=tycon, args=vs, tyexp=ty})
         |. keyword "type"
         |. whitespaceOrComment
         |= simpleType
         |. whitespaceOrComment
         |. operator "="
         |. whitespaceOrComment
         |= typeExpr


------------------------------------------------------------    
-- data type declarations
------------------------------------------------------------
dataDeclaration : Parser Decl
dataDeclaration
    = succeed makeDataDecl
         |. keyword "data"
         |. whitespaceOrComment
         |= simpleType
         |. whitespaceOrComment
         |. operator "="
         |. whitespaceOrComment
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
      , spaces = whitespaceOrComment
      , item = dataAlternative
      , trailing = Parser.Forbidden               
      }

dataAlternative : Parser (Name, List Type)
dataAlternative
    = succeed (\tag ts -> (tag,ts))
          |= upperIdentifier
          |. whitespace
          |= Parser.sequence
             { start = ""
             , end = ""
             , separator = ""
             , spaces = whitespace
             , item = atomicType
             , trailing = Parser.Forbidden                      
             }

            
---------------------------------------------------------------------
equation : Parser Decl
equation
    = getParseChomped_ equationLHS |>
      andThen (\((name,patts), prefix) ->
          succeed (makeEquation name patts)
              |. whitespace     
              |= equationAlts prefix
              |. whitespace
              |= whereBindings)

-- helper function to construct an equation 
makeEquation : Name -> List Pattern -> Matching -> List Bind -> Decl
makeEquation name patts match binds
    = Equation name (makePatterns patts (makeBindings binds match))
                      
whereBindings : Parser (List Bind)
whereBindings
    = oneOf
      [ succeed identity
          |. keyword "where"
          |. whitespace
          |= lazy (\_ -> indented bindings)
      , succeed []
      ]
                 


-- left hand side of an equation
equationLHS : Parser (Name, List Pattern)
equationLHS
    = topExpr
      |> andThen makeLHS
      |> andThen checkLHS

makeLHS : Expr -> Parser (Name, List Pattern)
makeLHS expr
    = case unwind expr of
          (Var fun, args) ->
              case translatePatterns args of
                  Ok patts ->
                      succeed (fun, patts)
                  Err msg ->
                      problem msg
          (BinaryOp fun e1 e2, []) ->
              case translatePatterns [e1,e2] of
                  Ok patts ->
                      succeed (fun, patts)
                  Err msg ->
                      problem msg
          _ ->
              problem "pattern bindings are not supported"

checkLHS : (Name, List Pattern) -> Parser (Name, List Pattern)
checkLHS (fun, patts)
    = if List.allDifferent (fun :: List.concatMap patternVars patts) then
          succeed (fun, patts)
      else
          problem "distinct pattern variables"

-- unwind a sequence of binary applications
unwind : Expr -> (Expr, List Expr)
unwind expr
    = unwind_ expr []

-- worker function
unwind_ : Expr -> List Expr -> (Expr, List Expr)
unwind_ expr args
    = case expr of
          App e0 e1 ->
              unwind_ e0 (e1::args)
          _ ->
              (expr, args)
         
        
      


-- equation alternatives
-- i.e. list of guards and expressions or a single return expression
equationAlts : String -> Parser Matching
equationAlts prefix
    = oneOf
      [ succeed (\alt alts -> joinAlts (alt::alts))
            |= equationGuard prefix
            |. whitespace
            |= Parser.sequence
               { start = ""
               , separator = ""
               , end = ""
               , spaces = whitespace
               , item = equationGuard prefix
               , trailing = Parser.Forbidden
               }
      , succeed (\(expr,info) -> Return expr (Just (prefix++info)))
           |= getParseChomped_ equationRHS
      ]
  
equationRHS : Parser Expr
equationRHS
    = succeed identity
           |. whitespace
           |. operator "="
           |. whitespace
           |= topExpr   


-- an equation guard, i.e. "| cond = expr"
equationGuard :  String -> Parser Matching
equationGuard prefix 
    = succeed (\((e1,e2), info) -> makeGuard e1 e2 (prefix++info))
         |= getParseChomped_ guardedExpr
              
guardedExpr : Parser (Expr, Expr)
guardedExpr
    = succeed Tuple.pair
          |. operator "|"
          |. whitespace
          |= topExpr
          |. whitespace
          |. operator "="
          |. whitespace
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
          Cons _ "True" [] ->
              Return expr (Just info)
          Cons _ "False" [] ->
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
         |. whitespace
         |. operator "::"
         |. whitespace
         |= typeExpr

-- principal non-terminal for type expressions
typeExpr : Parser Type
typeExpr
    = succeed insertArrows
           |= typeApplication
           |. whitespace
           |= oneOf
              [ Parser.sequence
                { start = "->"
                , end = ""
                , separator = "->"
                , item = typeApplication
                , spaces = whitespace
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
             , item = atomicType
             , spaces = spaces
             , trailing = Parser.Forbidden
             }
      , atomicType
      ]

           
          
-- atomic types
atomicType : Parser Type
atomicType
    = oneOf
      [ succeed (\c -> tyConst c [])
            |= upperIdentifier
      , succeed TyVar        -- identifiers are parsed as free type vars
            |= identifier    -- should be generalized if necessary
      , succeed tyList
            |. symbol "["
            |. whitespace
            |= lazy (\_ -> typeExpr)
            |. whitespace 
            |. symbol "]"
      , Parser.sequence
            { start = "("
            , end = ")"
            , separator = ","
            , item = lazy (\_ -> typeExpr)
            , spaces = whitespace
            , trailing = Parser.Forbidden
            } |> andThen makeTupleType
      ]

makeTupleType : List Type -> Parser Type
makeTupleType ts
    = case ts of
          [] ->
              succeed tyUnit
          [t1] ->
              succeed t1
          [t1,t2] ->
              succeed (tyPair t1 t2)
          [t1, t2, t3] ->
              succeed (tyTuple3 t1 t2 t3)
          [t1, t2, t3, t4] ->
              succeed (tyTuple4 t1 t2 t3 t4)
          _ ->
              problem "tuple type with at most 4 elements" 
              
              
          
               
------------------------------------------------------------------
-- auxiliary parsers for expressions
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


             
atomicPattern : Parser Pattern
atomicPattern
    = atomicExpr |> andThen patternFromExpr

pattern : Parser Pattern
pattern
    = topExpr |> andThen patternFromExpr
                             
    
-- top-level expressions
topExprEnd : Parser Expr
topExprEnd
    = succeed identity
        |= topExpr
        |. Parser.end

  
topExpr : Parser Expr
topExpr = infix0

-- dummy operator for @-patterns
infix10 = infixNonAssoc applicativeExpr [ ("@", BinaryOp "@") ]
          
-- we shouldn't really allow mixing infixl and infixr level 9 operators
infix9r = infixRight infix10 [ (".", \e1 e2 ->  App (App (Var ".") e1) e2) ]
infix9 = infixLeft infix9r [ ("!!", \e1 e2 -> App (App (Var "!!") e1) e2) ]
infix7 = infixLeft infix9 [ ("*", BinaryOp "*"), ("/", BinaryOp "/") ]
infix6 = infixLeft infix7  [ ("+", BinaryOp "+")
                            , ("-", BinaryOp "-") ]
infix5 = infixRight infix6 [ (":", \e1 e2 -> Cons False ":" [e1,e2])
                           , ("++", \e1 e2 -> App (App (Var "++") e1) e2)
                           ]
infix4 = infixNonAssoc infix5 [ ("==", BinaryOp "==")
                              , ("/=", BinaryOp "/=")
                              , ("<=", BinaryOp "<=")
                              , (">=", BinaryOp ">=")
                              , ("<", BinaryOp "<")
                              , (">", BinaryOp ">")
                              ] 

infix3 = infixRight infix4 [ ("&&", \e1 e2 -> App (App (Var "&&") e1) e2) ]
infix2 = infixRight infix3 [ ("||", \e1 e2 -> App (App (Var "||") e1) e2) ]
infix0 = infixRight infix2 [ ("$", \e1 e2 -> App (App (Var "$") e1) e2)
                           , ("$!", \e1 e2 -> App (App (Var "$!") e1) e2)
                           ]

           

        
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
    = let continue expr
             = oneOf <|
               List.map (\(op,func) ->
                             succeed (\item -> Parser.Loop (func expr item))
                                |. whitespace_
                                |. operator op
                                |. whitespace_
                                |= operand) table
                   ++ [succeed (Parser.Done expr)]
      in operand |> andThen (\start -> Parser.loop start continue)

              
-- parse infix right associative operators
--
infixRight : Parser Expr -> List (Name, BinOp) -> Parser Expr
infixRight operand table
    = let go acc
           = oneOf <|
             List.map (\(op,func) ->
                             succeed (\item ->
                                          Parser.Loop ((func,item)::acc))
                                |. whitespace_
                                |. operator op
                                |. whitespace_
                                |= operand) table
                   ++ [succeed (Parser.Done (List.reverse acc))]
      in operand |> andThen (\start -> Parser.loop [] go
                 |> andThen (\list -> succeed (applyRight list start)))

applyRight : List (a -> a -> a, a) -> a -> a
applyRight lst x
    = case lst of
          [] -> x
          ((f,y)::fs) ->  f x (applyRight fs y)
                               

-- parse non-associative operators
--
infixNonAssoc : Parser Expr -> List (Name, BinOp) -> Parser Expr
infixNonAssoc operand table 
    = operand |> andThen
      (\left ->
           spaces |> andThen (\_ ->
           oneOf
           [ succeed (\func right -> func left right)
              |= oneOf
                  (List.map (\(op,func) -> succeed func
                                      |. operator op) table)
              |. spaces          
              |= operand
           , succeed left
           ]))
           

-- parethesised operators and sections          
operatorSections : Parser Expr
operatorSections  
    = succeed identity
        |. symbol "("
        |= oneOf
           [ backtrackable <|
                 succeed Var
                   |= infixOperator
                   |. symbol ")"
           , backtrackable <|
                 succeed (\e1 op -> App (Var op) e1)
                  |= lazy (\_ -> application)
                  |. spaces
                  |= leftSection
                  |. symbol ")"
           , backtrackable <|
               succeed (\op e2 -> App (App (Var "flip") (Var op)) e2)
                  |= rightSection 
                  |. spaces
                  |= lazy (\_ -> application)
                  |. symbol ")"
           ]
    

-- left section operator 
leftSection : Parser String
leftSection
    = oneOf
      [ infixOperator
      , infixFunction
      ]


rightSection : Parser String
rightSection
    = oneOf
      [ rightOperator
      , infixFunction
      ]
      
    
-- right section operator; disallow (- exp) as a section
rightOperator : Parser String
rightOperator
    = Parser.chompWhile (\c -> c/='-' && AST.operatorChar c)
    |> Parser.getChompedString
    |> andThen (\s -> if String.isEmpty s
                      then problem "operator"
                      else succeed s)

infixFunction : Parser String
infixFunction
    = succeed identity
          |. symbol "`"
          |= identifier
          |. symbol "`"   
                
    
              

applicativeExpr : Parser Expr
applicativeExpr
    = oneOf [ if_then_else,
                  letExpr,
                  caseExpr,
                  lambdaExpr,
                  prefixNeg,
                  backtrackable infixApp,
                  application ]


-- atomic expressions
--
atomicExpr : Parser Expr
atomicExpr =
    oneOf
    [  succeed (notImplemented "floats are not supported")
          |= backtrackable floatLiteral
    , succeed Number
          |= intLiteral
    , succeed Var  
          |= identifier
    , succeed Char
          |= charLiteral
    , succeed stringToList
          |= stringLiteral
    , succeed (\tag -> Cons True tag [])
          |= upperIdentifier
    , backtrackable <|
        succeed (UnaryOp "!" << Var)
           |. token "!"
           |= identifier
    , backtrackable operatorSections
    , listLike
    , literalTuple
    ]


-- parser for "list-like" things
-- enumerations, list comprehensions and literal lists
listLike :  Parser Expr
listLike
    = succeed identity
             |. symbol "["
             |= oneOf
                [ backtrackable <|
                      succeed (App (Var "enumFrom"))
                       |= lazy (\_ -> topExpr)
                       |. symbol ".."
                       |. spaces
                       |. symbol "]"
                , backtrackable <|
                    succeed (\e1 e2 -> makeApp (Var "enumFromThen") [e1,e2])
                       |= lazy (\_ -> topExpr)
                       |. symbol ","
                       |. spaces
                       |= lazy (\_ -> topExpr)
                       |. symbol ".."
                       |. spaces
                       |. symbol "]"
                , backtrackable <|
                    succeed (\e1 e2 -> makeApp (Var "enumFromTo") [e1,e2])
                       |= lazy (\_ -> topExpr)
                       |. symbol ".."
                       |. spaces
                       |= lazy (\_ -> topExpr)
                       |. spaces
                       |. symbol "]"   
                , backtrackable <|
                    succeed (\e1 e2 e3 -> makeApp (Var "enumFromThenTo") [e1,e2,e3])
                       |= lazy (\_ -> topExpr)
                       |. symbol ","
                       |. spaces
                       |= lazy (\_ -> topExpr)
                       |. symbol ".."
                       |. spaces
                       |= lazy (\_ -> topExpr)
                       |. spaces
                       |. symbol "]"
                , succeed (\(e,qs) -> ListComp e qs)
                       |= backtrackable listComp
                , succeed AST.listLit
                       |= Parser.sequence
                          { start = ""
                          , end = "]"
                          , separator = ","
                          , spaces = whitespace
                          , item = lazy (\_ -> topExpr)
                          , trailing = Parser.Forbidden
                          }
                ]

-- parser for list comprehensions;
-- just for reporting better error messages
listComp : Parser (Expr, List Qual)
listComp
    = succeed Tuple.pair
          |. whitespace    
          |= lazy (\_ -> topExpr)
          |. whitespace
          |= Parser.sequence
             { start = "|"
             , spaces = whitespace
             , separator = ","
             , end = "]"
             , item = listQual
             , trailing = Parser.Forbidden
             }

listQual : Parser Qual
listQual            
    = oneOf
      [ backtrackable <|
            succeed Gen
               |= lazy (\_ -> pattern)
               |. whitespace
               |. symbol "<-"
               |. whitespace
               |= lazy (\_ -> topExpr)
      , succeed LetQual
               |. keyword "let"
               |. whitespace
               |= identifier
               |. whitespace
               |. operator "="
               |. whitespace
               |= lazy (\_ -> topExpr)
      , succeed Guard
               |= lazy (\_ -> topExpr)
      ]
            

               

literalTuple : Parser Expr
literalTuple
    = Parser.sequence
      { start = "("
      , end = ")"
      , separator = ","
      , spaces = whitespace
      , item = lazy (\_ -> topExpr)
      , trailing = Parser.Forbidden
      } |> andThen makeTuple

makeTuple : List Expr -> Parser Expr
makeTuple args
    = case args of
          [] ->
              succeed (Cons True "()" [])
          [e] ->
              succeed e -- no singleton tuple
          [e1,e2] ->
              succeed (Cons False "," args)
          [e1,e2,e3] ->
              succeed (Cons False ",," args)
          [e1,e2,e3,e4] ->
              succeed (Cons False ",,," args)          
          _ ->
              problem "tuple with at most 4 elements"

-- build an application to a list of arguments
makeApp : Expr -> List Expr -> Expr
makeApp e0 args
    = case (e0, args) of
          (Cons _ tag args1, args2) ->
              Cons False tag (args1 ++ args2)
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
         |= atomicExpr

           
             
infixApp : Parser Expr
infixApp
    = succeed (\e1 f e2 -> BinaryOp f e1 e2)
         |= application 
         |. spaces   
         |= infixFunction
         |. spaces   
         |= application 
             
application : Parser Expr       
application
    = succeed makeApp
         |= atomicExpr
         |. whitespace_
         |= Parser.sequence
            { start = ""
            , end = ""
            , separator = ""
            , spaces = whitespace_
            , item = atomicExpr
            , trailing = Parser.Forbidden
            }

-- whitespace indented further right than the current level
whitespace_ : Parser ()
whitespace_
    = oneOf [ backtrackable (whitespace |> andThen (\_ -> checkIndent (<)))
            , succeed () ]
    
lambdaExpr : Parser Expr
lambdaExpr
    = getParseChomped lambdaExprAux 
      
lambdaExprAux : Parser (String -> Expr)  
lambdaExprAux
    = succeed (\patts expr info ->
                   AST.lambda Nothing
                       (makePatterns patts (Return expr (Just info))))
         |. symbol "\\"
         |. whitespace
         |= Parser.sequence
            { start = ""
            , end = ""
            , separator = ""
            , spaces = whitespace
            , item = atomicPattern
            , trailing = Parser.Forbidden
            }
         |. symbol "->"
         |. whitespace
         |= lazy (\_ -> topExpr)

-- add a list of patterns to a matching 
makePatterns : List Pattern -> Matching -> Matching
makePatterns ps end
    = List.foldr Match end ps


letExpr : Parser Expr
letExpr
    = succeed Let
         |. keyword "let"
         |. whitespace
         |= lazy (\_ -> indented bindings)
         |. whitespace
         |. keyword "in"
         |. whitespace
         |= lazy (\_ -> topExpr)


caseExpr : Parser Expr
caseExpr
    = succeed Case
         |. keyword "case"
         |. whitespace
         |= lazy (\_ -> topExpr)
         |. whitespace
         |. keyword "of"
         |. whitespace
         |= indented caseAlts

caseAlts : Parser (List (Pattern,Expr,Info))
caseAlts
    = sepBy1 caseAlt semicolon
           

caseAlt : Parser (Pattern, Expr, Info)
caseAlt
    = getParseChomped <|
      succeed (\patt expr info -> (patt,expr, info))
         |= lazy (\_ -> pattern)
         |. whitespace
         |. symbol "->"
         |. whitespace
         |= lazy (\_ -> topExpr)
            
    
-- a sequence of indented bindings
bindings : Parser (List Bind)
bindings
    = succeed collectBinds
         |= declList
      |> andThen (\binds -> case checkBinds binds of
                                Err msg ->
                                    problem msg
                                Ok _ ->
                                    succeed binds)

            
if_then_else : Parser Expr       
if_then_else
    = succeed IfThenElse
         |. keyword "if"
         |. whitespace
         |= lazy (\_ -> topExpr)
         |. whitespace
         |. keyword "then"
         |. whitespace
         |= lazy (\_ -> topExpr)
         |. whitespace
         |. keyword "else"
         |. whitespace
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
                   

intLiteral : Parser Int
intLiteral
    = Parser.getChompedString integral |> andThen
      (\s -> case String.toInt s of
                 Nothing ->
                     problem "integer literal"
                 Just n ->
                     succeed n)

floatLiteral : Parser String
floatLiteral
    = Parser.getChompedString floating
          
integral : Parser ()
integral
    = Parser.chompIf Char.isDigit |. Parser.chompWhile Char.isDigit
    
floating : Parser ()
floating = integral
           |. symbol "."
           |. integral
           |. oneOf [exponent, succeed ()]

exponent : Parser ()
exponent
    = Parser.chompIf (\x -> x == 'e' || x == 'E')
      |. oneOf [ Parser.chompIf (\x -> x == '+' || x == '-')
               , succeed ()
               ]
      |. integral

       
  
    
-- lower case identifiers    
identifier : Parser String
identifier
    = variable
      { start = \c -> Char.isLower c || c == '_'
      , inner = \c -> Char.isAlphaNum c || c=='_' || c == '\''
      , reserved = reservedWords
      }

reservedWords : Set String    
reservedWords
    = Set.fromList [ "if", "then", "else", "let", "in", "case", "of",
                         "where", "data", "type" ]

   
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
        |. token "'"
        |= character '\''
        |. token "'"

           
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
                  |. token "\\" 
                  |= escapeChar
            , getChompedChar (Parser.chompIf (\c -> c/=delimiter))
            ]

escapeChar : Parser Char
escapeChar
    = oneOf [ succeed '\n'
                  |. token "n"
            , succeed '\t'
                  |. token "t"
            , getChompedChar
                  (Parser.chompIf (\c -> c == '\\' || c=='\'' || c=='\"'))
            , succeed Char.fromCode
                  |= Parser.int
            ]
      
getChompedChar : Parser a -> Parser Char
getChompedChar p
    = Parser.getChompedString p |>
      andThen (\s -> case String.uncons s of
                         Just (c, "") -> succeed c
                         _ -> problem "character literal")
      
                  


-- consume whitespaces (including newlines) or comments
whitespaceOrComment : Parser ()
whitespaceOrComment
    = Parser.loop 0 <| ifProgress <|
          oneOf
          [ Parser.Workaround.lineCommentAfter "--"
          , Parser.multiComment "{-" "-}" Parser.Nestable
          , whitespace
          ]

-- whitespace, including newlines
whitespace : Parser ()
whitespace    
    = Parser.chompWhile (\c -> c==' ' || c=='\t' || c=='\n' || c=='\r')

-- just spaces and tabulations (no newlines)
spaces : Parser ()
spaces
    = Parser.chompWhile (\c -> c==' ' || c=='\t')

semicolon : Parser ()
semicolon
    = oneOf [symbol ";", checkIndent (==)]

--
-- compare the the indentation level  against the current column
--
checkIndent : (Int -> Int -> Bool) -> Parser ()
checkIndent cmp =
  succeed (\indent column -> cmp indent column)
    |= Parser.getIndent
    |= Parser.getCol
    |> andThen (\b -> if b then succeed ()
                      else problem "indentation")
       
--
-- parse a sequence of items separated by a given parser
--
sepBy1 : Parser a -> Parser () -> Parser (List a)
sepBy1 itemp separator
    = let next items =
              oneOf [ succeed (\item -> Parser.Loop (item::items))
                          |. whitespace
                          |. separator
                          |. whitespace
                          |= itemp
                    , succeed (Parser.Done (List.reverse items))
                    ]
      in 
          itemp |> andThen (\item -> Parser.loop [item] next)
              
      
--      
-- Remember the current column indentation level and run a parser
--
indented : Parser a -> Parser a
indented parser
    = Parser.getCol |> andThen (\col -> Parser.withIndent col parser)
 


      
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
ifProgress parser offset
    = succeed (\newOffset -> if offset == newOffset
                             then Parser.Done ()
                             else Parser.Loop newOffset)
           |. parser
           |= Parser.getOffset
      

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
          List.map (\(first,rest) ->
                        let probs = List.map .problem (first::rest)
                        in 
                            "line " ++ String.fromInt first.row  ++ "," ++
                            "col " ++ String.fromInt first.col ++ ", " ++
                            "expecting: " ++
                                String.join ", "
                                    (Set.toList <| Set.fromList <|
                                         List.filterMap problemToString probs)
                   ) groups
              
problemToString : Parser.Problem -> Maybe String
problemToString prob
    = case prob of
          Parser.Expecting s ->
              if s == "!" then Nothing else Just (quote s)
          Parser.ExpectingSymbol s ->
              if s == "!" then Nothing else Just (quote s)
          Parser.ExpectingKeyword s ->  Just s
          Parser.ExpectingInt -> Just "integer"
          Parser.ExpectingHex -> Just "hexadecimal"
          Parser.ExpectingOctal -> Just "octal"
          Parser.ExpectingBinary -> Just "binary"
          Parser.ExpectingFloat -> Just "float"
          Parser.ExpectingNumber -> Just "number"
          Parser.ExpectingVariable -> Just "variable"
          Parser.ExpectingEnd -> Just "end of input"
          Parser.Problem s ->  Just s
          _ -> Nothing

quote : String -> String
quote = String.replace "," "comma" >>
        String.replace "\'" "single quote" >>
        String.replace "\"" "double quote" >>
        String.replace "[" "open list" >>
        String.replace "]" "close list"      

-- check if an expression can be converted to a pattern               
patternFromExpr : Expr -> Parser Pattern
patternFromExpr expr
    = case translatePattern expr of
          Ok patt ->
              if List.allDifferent (patternVars patt) then
                  succeed patt
              else
                  problem "pattern variables must be distinct"
          Err msg ->
              problem msg
               
-- convert an expression into a pattern or fail
translatePattern : Expr -> Result String Pattern
translatePattern expr
    = case expr of
          Var x ->
              Ok (if x == "_" then DefaultP else VarP x)
          UnaryOp "!" (Var x) ->
              Ok (BangP x)
          Number n ->
              Ok (NumberP n)
          Char c ->
              Ok (CharP c)
          Cons _ c args ->
              translatePatterns args |> Result.andThen (Ok << ConsP c)
          BinaryOp "@" (Var id) e2 ->
              translatePattern e2 |> Result.andThen (Ok << AsP id)
          _ ->
              Err ("invalid pattern: " ++ Shows.showExpr expr)

translatePatterns : List Expr -> Result String (List Pattern)
translatePatterns exprs
    = case exprs of
          [] ->
              Ok []
          (e::es) ->
              translatePattern e
                  |> Result.andThen (\p -> translatePatterns es
                  |> Result.andThen (\ps -> Ok (p::ps)))
