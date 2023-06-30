{-
  Abstract Syntax Trees for Haskelite, a small subset of Haskell
  Pedro Vasconcelos, 2021-23
-}
module AST exposing (..)

import Types exposing (Type)
import Dict exposing (Dict)
import Maybe

type alias Name
    = String

-- * expressions 
type Expr 
    = App Expr Expr
    | Lam (Maybe Name) Matching  
      -- a lambda is Just name if it was defined through a binding;
      -- otherwise the name field is Nothing
    | Var Name
    | Number Int
    | Cons Name (List Expr)
    | ListLit (List Expr)              -- [1,2,3]
    | TupleLit (List Expr)             -- (1,2,3)
    | InfixOp Name Expr Expr           --  primitive operators +, * etc
    | IfThenElse Expr Expr Expr
    | Error                            -- runtime errors
      
-- * matchings
type Matching 
    = Return Expr Info              -- matching succedeed
    | Fail                          -- matching failed
    | Match Pattern Matching        -- match a pattern
    | Arg Expr Matching             -- argument supply
    | Alt Matching Matching         -- alternative


-- * patterns      
type Pattern
    = DefaultP               -- ignore (_)
    | VarP Name
    | BangP Name             -- bang pattern (for strict evaluation)
    | NumberP Int
    | ListP (List Pattern)
    | TupleP (List Pattern)
    | ConsP Name (List Pattern)

      
-- * declarations      
type Decl
    = TypeSig Name Type
    | Equation Name Matching      

-- * bindings
type alias Bind
    = { name: Name, typeSig: Maybe Type, expr: Expr }
    
-- * programs
type Program
    = Letrec (List Bind) Expr

     
     
-- term substitutions
type alias Subst
    = Dict Name Expr
    
-- information associated with a reduction
type alias Info
    = String
      
declName : Decl -> Name
declName decl
    = case decl of
          TypeSig name _ -> name
          Equation name _ -> name


              
-- apply substitution to an expression
applySubst : Subst -> Expr -> Expr
applySubst s e
    = case e of
          Var x ->
              case Dict.get x s of
                  Nothing -> e
                  Just e1 -> e1
                             
          Lam optname m ->
              Lam optname (applyMatchSubst s m)
                      
          App e1 e2 ->
              App (applySubst s e1)(applySubst s e2)
                  
          InfixOp op e1 e2 ->
              InfixOp op (applySubst s e1) (applySubst s e2)
                  
          ListLit l ->
              ListLit (List.map (applySubst s) l)
                  
          TupleLit l ->
              TupleLit (List.map (applySubst s) l)

          Cons c args ->
              Cons c (List.map (applySubst s) args)

          IfThenElse e1 e2 e3 ->
              IfThenElse (applySubst s e1) (applySubst s e2) (applySubst s e3)

          _ ->
              e

{-                   
          Ctx e1 ->
              Ctx (applySubst s e1)
-}

                  
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
              
                  

-- list variables bound by a pattern
patternVars : Pattern -> List Name
patternVars patt 
    = case patt of
          VarP x ->
              [x]
          BangP x ->
              [x]
          ListP ps ->
              List.concatMap patternVars ps
          TupleP ps ->
              List.concatMap patternVars ps
          ConsP tag ps ->
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
          
      
                  
isOperator : Name -> Bool
isOperator = String.all operatorChar 
                  
operatorChar : Char -> Bool
operatorChar c =
    c=='!' || c=='+' || c=='*' || c=='-' || c=='>' || c=='<' ||
        c==':' || c=='=' || c=='&' || c=='|' || c=='.' 

trueCons : Expr
trueCons = Cons "True" []

falseCons : Expr           
falseCons = Cons "False" []           
             

            
