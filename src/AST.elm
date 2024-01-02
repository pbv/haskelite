{-
  Abstract Syntax Trees for Haskelite, a small subset of Haskell
  Pedro Vasconcelos, 2021-23
-}
module AST exposing (..)

import Types exposing (Type(..), Tycon)
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
      -- the integer field is the matching arity
      -- the maybe field is `Just name` if the lambda was defined through a binding;
      -- otherwise it is Nothing
    | Let (List Bind) Expr              -- local bindings; can be recursive
    | Case Expr (List (Pattern, Expr))
    | Var Name
    | Number Int
    | Char Char
    | Cons Tag (List Expr)
    | BinaryOp Name Expr Expr            -- binary primitive operations
    | UnaryOp Name Expr                  -- unary primitive operations
    | IfThenElse Expr Expr Expr
    | Error Expr                       -- runtime errors;
                                       -- argument should be an evaluated string

      
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
    = DefaultP                  -- ignore (_)
    | VarP Name                 -- variables
    | BangP Name                -- bang pattern (for strict evaluation)
    | NumberP Int               -- number pattern
    | CharP Char                -- character pattern
    | ConsP Tag (List Pattern)  -- constructors

      
-- * declarations      
type Decl
    = TypeSig Name Type            -- type signature
    | Equation Name Matching       -- a single equation
    | Data DataDecl                -- data declaration

-- * data declaration; alternatives in GADT-style      
type alias DataDecl
    = { result: Type, alternatives: List (Tag, Type) }
      
-- * bindings
type alias Bind
    = { name: Name, typeSig: Maybe Type, expr: Expr }

-- * modules
type alias Module
    = { dataDecls : List DataDecl
      , binds : List Bind
      , skip : List Name         -- names to skip over when evaluating
      }
      
-- * programs
type Program
    = LetProg Module Expr
     
-- term substitutions
type alias Subst
    = Dict Name Expr
      
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
                  Just e1 -> e1
                             
          Lam arity optname m ->
              Lam arity optname (applyMatchSubst s m)

          Let binds e0 ->
              let
                  s1 = restrictSubst (List.map .name binds) s
                  binds1 = applyBindsSubst s1 binds
              in
                  Let binds1 (applySubst s1 e0)

          Case e0 alts ->
              let e1 = applySubst s e0
              in Case e1 (applyAltsSubst s alts)
                      
          App e1 e2 ->
              App (applySubst s e1)(applySubst s e2)
                  
          BinaryOp op e1 e2 ->
              BinaryOp op (applySubst s e1) (applySubst s e2)
          UnaryOp op e1 ->
              UnaryOp op (applySubst s e1)
          Cons c args ->
              Cons c (List.map (applySubst s) args)

          IfThenElse e1 e2 e3 ->
              IfThenElse (applySubst s e1) (applySubst s e2) (applySubst s e3)

          Number _ ->
              e
          Char _ ->
              e
          Error _ ->
              e

                  
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

                      
applyAltsSubst : Subst -> List (Pattern,Expr) -> List (Pattern,Expr)
applyAltsSubst s 
    = List.map (\(patt,expr) -> let s1 = restrictSubst (patternVars patt) s
                                in (patt, applySubst s1 expr))
                  

applyBindsSubst : Subst -> List Bind -> List Bind
applyBindsSubst s 
    = List.map (\b -> { b | expr = applySubst s b.expr }) 
                  
restrictSubst : List Name -> Subst -> Subst
restrictSubst names s
    = List.foldr Dict.remove s names
                  

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
         c==':' || c=='=' || c=='&' || c=='|' || c=='.'  || c=='/'

-- AST constructors
trueCons : Expr
trueCons = Cons "True" []

falseCons : Expr           
falseCons = Cons "False" []           

lambda : Maybe Name -> Matching -> Expr
lambda optname m
    = Lam (matchingArity m) optname m
            
-- smart constructors for literal lists, strings and tuples
listLit : List Expr -> Expr
listLit = List.foldr (\x xs -> Cons ":" [x,xs]) (Cons "[]" [])

stringLit : String -> Expr
stringLit s = listLit (List.map Char <| String.toList s)
          
tupleLit : List Expr -> Expr
tupleLit args
    = case args of
          [e] -> e   -- no singleton tuple
          _ -> Cons "," args

listPattern : List Pattern -> Pattern
listPattern =
    List.foldr (\p ps -> ConsP ":" [p,ps]) (ConsP "[]" [])

tuplePattern : List Pattern -> Pattern
tuplePattern ps
    = case ps of
          [p] -> p          -- no singleton tuple
          _ -> ConsP "," ps
          
            
-- smart constructor for multi-arity applications
applyMany : Expr -> List Expr -> Expr
applyMany = List.foldl (\x y->App y x) 


              
