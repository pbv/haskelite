{-
  Abstract Syntax Trees for Haskelite, a small subset of Haskell
  Pedro Vasconcelos, 2021
-}
module AST exposing (..)

import Dict exposing (Dict)
import Maybe

type alias Name
    = String

-- * expressions
type Expr
    = App Expr (List Expr)    -- non-empty list of arguments
    | Lam (List Name) Expr    -- non-empty list of variables
    | Var Name
    | Number Int
    | Boolean Bool
    | ListLit (List Expr)           -- [1,2,3]
    | TupleLit (List Expr)          -- (1,2,3)
    | InfixOp Name Expr Expr        --  operators +, * etc
    | IfThenElse Expr Expr Expr
    | Fail String                 -- runtime errors
    | Eval Expr                   -- evaluation context

-- * declarations      
type Decl
    = TypeSig Name Type
    | Equation Name (List Pattern) Expr 

-- * patterns      
type Pattern
    = VarP Name
    | BangP Name             -- bang pattern (for strict evaluation)
    | BooleanP Bool
    | NumberP Int
    | ListP (List Pattern)
    | ConsP Pattern Pattern
    | TupleP (List Pattern)

-- a single alternative
type alias Alt
    = (List Pattern, Expr)


-- * programs
type Program
    = Letrec (List Bind) Expr

-- * bindings
type alias Bind
    = { name: Name, typeSig: Maybe Type, alts: List Alt }

      
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

    
-- information line associated with a reduction
type Info
    = Prim String   -- primitive operation
    | Rewrite Decl  -- rewrite rule 

      
declName : Decl -> Name
declName decl
    = case decl of
          TypeSig name _ -> name
          Equation name _ _ -> name

                               
-- remove evaluation context marker
uneval : Expr -> Expr
uneval expr =
    case expr of
        App e1 args ->
            App (uneval e1) (List.map uneval args)

        Lam x e1 ->
            Lam x (uneval e1)

        ListLit es ->
            ListLit (List.map uneval es)

        TupleLit es ->
            TupleLit (List.map uneval es)

        InfixOp op e1 e2 ->
            InfixOp op (uneval e1) (uneval e2)

        IfThenElse e1 e2 e3 ->
            IfThenElse (uneval e1) (uneval e2) (uneval e3)
                
        Eval e ->
            e

        _ ->
            expr

-- perform variable substitution
applySubst : Subst -> Expr -> Expr
applySubst s e
    = case e of
          Number _ ->  e
                       
          Boolean _ -> e
                       
          Var x ->
              case Dict.get x s of
                  Nothing -> e
                  Just e1 -> e1
                             
          Lam xs e1 ->
              let
                  s1 = List.foldr Dict.remove s xs
              in
                  Lam xs (applySubst s1 e1)
                      
          App e1 args ->
              App (applySubst s e1) <| List.map (applySubst s) args
                  
          InfixOp op e1 e2 ->
              InfixOp op (applySubst s e1) (applySubst s e2)
                  
          ListLit l ->
              ListLit (List.map (applySubst s) l)
                  
          TupleLit l ->
              TupleLit (List.map (applySubst s) l)

          IfThenElse e1 e2 e3 ->
              IfThenElse (applySubst s e1) (applySubst s e2) (applySubst s e3)

          Fail _ -> e

          Eval e1 ->
              Eval (applySubst s e1)





