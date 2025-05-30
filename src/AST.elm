{-
  Abstract Syntax Trees for Haskelite, a small subset of Haskell
  Pedro Vasconcelos, 2021-23
-}
module AST exposing (..)

import Types exposing (Type(..), Tycon, Tyvar)
import Dict exposing (Dict)
import Maybe

-- * variables (terms and types)
type alias Name
    = String

-- * constructor tags      
type alias Tag
    = String

-- * information associated with a successful match
type alias Info
    = String     
      
-- * expressions 
type Expr 
    = App Expr Expr
    | Lam Int (Maybe Name) Matching
      -- the integer field is the matching arity the maybe field is
      -- `Just name` if the lambda was defined through a binding;
      -- otherwise it is Nothing
    | Let (List Bind) Expr              -- local bindings; can be recursive
    | Case Expr (List (Pattern, Expr, Info))
    | Var Name
    | Number Int
    | Char Char
      -- boolean flag to signal if constructor is normalized
      -- only normalized constructors are whnfs
    | Cons Bool Tag (List Expr)
    | BinaryOp Name Expr Expr            -- binary primitive operations
    | UnaryOp Name Expr                  -- unary primitive operations
    | IfThenElse Expr Expr Expr
    | Exception String                   -- runtime errors
    | ListComp Expr (List Qual)          -- list comprehensions
    | Unimplemented NotImplemented       -- unimplemented language feature
    | Marked Expr                        -- mark around an expressoin

      

-- comprehension qualifiers
type Qual
    = Gen Pattern Expr    -- p <- e 
    | Guard Expr          -- boolean expr
    | LetQual Name Expr   -- let x = e

type alias NotImplemented = { source : String, message : String }
    
notImplemented : String -> String -> NotImplemented
notImplemented msg src = {source=src, message=msg}
      
    
-- * matchings
type Matching 
    = Return Expr (Maybe Info)      -- matching succeeded
    | Fail                          -- matching failed
    | Match Pattern Matching        -- match a pattern
    | Arg Expr Matching             -- supply an argument
    | Alt Matching Matching         -- alternative
    | Where (List Bind) Matching    -- where bindings; can be recursive


-- * patterns      
type Pattern
    = DefaultP                  -- wildcard (_)
    | VarP Name                 -- variables
    | BangP Name                -- bang pattern (for strict evaluation)
    | AsP Name Pattern          -- as pattern (x@pat)
    | NumberP Int               -- number pattern
    | CharP Char                -- character pattern
    | ConsP Tag (List Pattern)  -- constructors

      
-- * declarations      
type Decl
    = TypeSig Name Type            -- type signature
    | Equation Name Matching       -- a single equation
    | Data DataDecl                -- data declaration
    | Alias AliasDecl              -- type synonym declaration

-- * data type declaration; alternatives in GADT-style      
type alias DataDecl
    = { tycon: Tycon, args: List Tyvar, alternatives: List (Tag, Type) }

-- * type synonym (alias)
type alias AliasDecl
    = { tycon: Tycon, args: List Tyvar, tyexp: Type }
      
-- * bindings
type alias Bind
    = { name: Name, typeSig: Maybe Type, expr: Expr }

-- * modules
type alias Module
    = { dataDecls : List DataDecl
      , aliasDecls : List AliasDecl
      , binds : List Bind
      }
      
-- * programs
type Program
    = LetProg Module Expr
     
-- name substitutions
type alias Subst
    = Dict Name Name
      
-- get the identifier associated with a declaration      
declName : Decl -> Maybe Name
declName decl
    = case decl of
          TypeSig name _ ->
              Just name
          Equation name _ ->
              Just name
          _ ->
              Nothing
              
-- apply substitution to an expression
applySubst : Subst -> Expr -> Expr
applySubst s e
    = case e of
          Var x ->
              case Dict.get x s of
                  Nothing -> e
                  Just y -> Var y
                             
          Lam arity optname m ->
              Lam arity optname (applyMatchSubst s m)

          Let binds e0 ->
              let
                  s1 = restrictSubst (List.map .name binds) s
                  binds1 = applyBindsSubst s1 binds
              in
                  Let binds1 (applySubst s1 e0)

          Case e0 alts ->
              Case (applySubst s e0) (applyAltsSubst s alts)
                      
          App e1 e2 ->
              App (applySubst s e1) (applySubst s e2)
                  
          BinaryOp op e1 e2 ->
              BinaryOp op (applySubst s e1) (applySubst s e2)
          UnaryOp op e1 ->
              UnaryOp op (applySubst s e1)
          Cons n c args ->
              Cons n c (List.map (applySubst s) args)

          IfThenElse e1 e2 e3 ->
              IfThenElse (applySubst s e1) (applySubst s e2) (applySubst s e3)

          Number _ ->
              e
          Char _ ->
              e
          Exception _ ->
              e
          ListComp e1 qs ->
              ListComp (applySubst s e1) (applyQualSubst s qs)
          Unimplemented _ ->
              e
          Marked e1 ->
              Marked (applySubst s e1)

                  
applyMatchSubst : Subst -> Matching -> Matching
applyMatchSubst s m
    = case m of          
          Return e info ->
              Return (applySubst s e) info

          Fail  ->
              Fail 

          Match p m1 ->
              let
                  s1 = List.foldr Dict.remove s (patternVars p)
              in 
                  Match p (applyMatchSubst s1 m1)

          Arg e m1 ->
              Arg (applySubst s e) (applyMatchSubst s m1)

          Alt m1 m2 ->
              Alt (applyMatchSubst s m1) (applyMatchSubst s m2)

          Where binds m2 ->
              let
                  s1 = restrictSubst (List.map .name binds) s
                  binds1= applyBindsSubst s1 binds
              in
                  Where binds1 (applyMatchSubst s1 m2)

                      
applyAltsSubst : Subst -> List (Pattern, Expr, info)
               -> List (Pattern,Expr, info)
applyAltsSubst s 
    = List.map (\(patt,expr,info) ->
                    let s1 = restrictSubst (patternVars patt) s
                    in (patt, applySubst s1 expr, info))
                  

applyBindsSubst : Subst -> List Bind -> List Bind
applyBindsSubst s 
    = List.map (\b -> { b | expr = applySubst s b.expr }) 
                  
restrictSubst : List Name -> Subst -> Subst
restrictSubst names s
    = List.foldr Dict.remove s names


applyQualSubst : Subst -> List Qual -> List Qual
applyQualSubst s qs
    = case qs of
          [] ->
              []
          (Gen p e :: qs1) ->
              let s1 = restrictSubst (patternVars p) s
              in Gen p (applySubst s e) :: applyQualSubst s1 qs1
          (Guard e :: qs1) ->
              Guard (applySubst s e) :: applyQualSubst s qs1
          (LetQual x e :: qs1) ->
              let s1 = restrictSubst [x] s
              in LetQual x e :: applyQualSubst s1 qs1 
              
-- list variables bound by a pattern
patternVars : Pattern -> List Name
patternVars patt 
    = case patt of
          VarP x ->
              [x]
          BangP x ->
              [x]
          ConsP c ps ->
              List.concatMap patternVars ps
          AsP x p ->
              x :: patternVars p
          _ ->
              []
                      


-- compute the arity of a matching
matchingArity : Matching  -> Int
matchingArity m
    = case m of
          Return _ _ ->
              0

          Fail ->
              0

          Match _ m1 ->
              1 + matchingArity m1

          Arg _ m1 ->
              max 0 (matchingArity m1 - 1)
                        
          Alt m1 _ ->
              matchingArity m1
              -- assumes both arities are equal, i.e.
              -- matching is well formed

          Where _ m2 ->
              matchingArity m2


-- check that a matching is well formed, i.e.
-- all branches of alternatives have identical arities
matchingWellformed : Matching -> Maybe Int
matchingWellformed m
    = case m of
          Return _ _ ->
              Just 0

          Fail ->
              Just 0

          Match _ m1 ->
              matchingWellformed m1 |>
              Maybe.andThen (\a -> Just (1+a))

          Arg _ m1 ->
              matchingWellformed m1 |>
              Maybe.andThen (\a -> Just (max 0 (a-1)))
                        
          Alt m1 m2 ->
              matchingWellformed m1 |>
              Maybe.andThen (\a1 -> matchingWellformed m2 |>
              Maybe.andThen (\a2 -> if a1==a2 then Just a1 else Nothing))
              -- both arities should be equal

          Where _ m2 ->
              matchingWellformed m2
               
                 
isOperator : Name -> Bool
isOperator = String.all operatorChar 
             
operatorChar : Char -> Bool
operatorChar c =
     c=='+' || c=='*' || c=='-' || c=='>' || c=='<' ||
     c==':' || c=='=' || c=='&' || c=='|' || c=='.' ||
     c=='/' || c=='!' || c=='^' || c=='$' || c=='!' ||
     c=='@'

-- AST constructors
trueCons : Expr
trueCons = Cons True "True" []

falseCons : Expr           
falseCons = Cons True "False" []           

lambda : Maybe Name -> Matching -> Expr
lambda optname m
    = Lam (matchingArity m) optname m
            
-- smart constructors for literal lists, strings and tuples
listLit : List Expr -> Expr
listLit = List.foldr (\x xs -> Cons False ":" [x,xs]) (Cons True "[]" [])

stringLit : String -> Expr
stringLit s = listLit (List.map Char <| String.toList s)

stringUnlit : Expr -> String
stringUnlit expr
    = stringUnlist expr []
              
stringUnlist : Expr -> List Char -> String
stringUnlist expr acc
    = case expr of
          Cons _ ":" [Char c, rest] ->
              stringUnlist rest (c::acc)
          _ ->
              String.fromList (List.reverse acc)
                     
            
-- smart constructor for multi-arity applications
applyMany : Expr -> List Expr -> Expr
applyMany = List.foldl (\x y->App y x) 
              
--
-- syntax translations
--
translateIfThenElse : Expr -> Expr -> Expr -> Matching
translateIfThenElse e1 e2 e3
    = Alt (Arg e1 (Match (ConsP "True" [])
                       (Return e2 (Just "if True"))))
           (Return e3 (Just "if False"))

translateCase : Expr -> List (Pattern,Expr,Info) -> Matching
translateCase e0 alts
    = let
        body = List.foldr
                 (\(patt,expr,info) rest ->
                      Alt (Match patt (Return expr (Just info))) rest)
                   Fail alts
      in Arg e0 body


      
                  
