{-
  Abstract Syntax Trees for Haskelite, a small subset of Haskell
  Pedro Vasconcelos, 2021
-}
module AST exposing (..)

import Dict exposing (Dict)
import Maybe

type alias Name
    = String

type alias Tag
    = String
      
-- * expressions 
type Expr 
    = App Expr Expr
    | Lam (Maybe Name) Matching  
      -- named lambda if it was defined in a binding
    | Var Name
    | Number Int
    | Cons Tag (List Expr)
    | ListLit (List Expr)                         -- [1,2,3]
    | TupleLit (List Expr)                        -- (1,2,3)
    | InfixOp Name Expr Expr                      --  primitive operators +, * etc
    | IfThenElse Expr Expr Expr
    | Error                                       -- runtime errors
      
-- * matchings
type Matching 
    = Return Expr Info              -- matching succedeed
    | Fail                          -- matching failed
    | Match Pattern Matching        -- match a pattern
    | Arg Expr Matching             -- argument supply
    | Alt Matching Matching         -- alternative

      
-- * declarations      
type Decl
    = TypeSig Name Type
    | Equation Name Matching

-- * patterns      
type Pattern
    = VarP Name
    | BangP Name             -- bang pattern (for strict evaluation)
    | NumberP Int
    | ListP (List Pattern)
    | TupleP (List Pattern)
    | ConsP Tag (List Pattern)


-- * programs
type Program
    = Letrec (List Bind) Expr

-- * bindings
type alias Bind
    = { name: Name, typeSig: Maybe Type, expr: Expr }

      
-- * types
type Type
    = TyVar Name         -- free type variable
    | TyGen Int          -- quantified (generic) type variable
    | TyBool
    | TyInt
    | TyTuple (List Type)
    | TyList Type
    | TyFun Type Type
     
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


{-
-- remove evaluation context marker
removeCtx : Expr -> Expr
removeCtx expr =
    case expr of
        App e1 args ->
            App (removeCtx e1) (List.map removeCtx args)

        Lam m ->
            Lam (removeMatchCtx m)

        ListLit es ->
            ListLit (List.map removeCtx es)

        TupleLit es ->
            TupleLit (List.map removeCtx es)

        InfixOp op e1 e2 ->
            InfixOp op (removeCtx e1) (removeCtx e2)

        IfThenElse e1 e2 e3 ->
            IfThenElse (removeCtx e1) (removeCtx e2) (removeCtx e3)
                
        Ctx e ->
            e

        Var x ->           
            Var x        -- NB: these have a different type than the lhs
                         -- hence the duplication
        Number n ->
            Number n

        Boolean b ->
            Boolean b

        Error  ->
            Error 
          

removeMatchCtx : Matching -> Matching
removeMatchCtx m
    = case m of
          Return expr info ->
              Return (removeCtx expr) info

          Fail  ->
              Fail 

          Match patt m1 ->
              Match patt (removeMatchCtx m1)
                  
          Arg expr m1 ->
              Arg (removeCtx expr) (removeMatchCtx m1)

          Alt m1 m2 ->
              Alt (removeMatchCtx m1) (removeMatchCtx m2)
-}
                
               
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
                      


-- * compute the arity of a matching
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
                        
          Alt m1 m2 ->
              max (matchingArity m1) (matchingArity m2)
                  -- both arities should be equal

isOperator : Name -> Bool
isOperator = String.all operatorChar 
                  
operatorChar : Char -> Bool
operatorChar c =
    c=='!' || c=='+' || c=='*' || c=='-' || c=='>' || c=='<' ||
        c==':' || c=='=' || c=='&' || c=='|' || c=='.' 
          
             
