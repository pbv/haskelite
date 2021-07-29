
module AST exposing (..)

import Dict exposing (Dict)
import Maybe

type alias Name
    = String

type Expr
    = App Expr Expr
    | Lam Name Expr
    | Var Name
    | Number Int
    | Boolean Bool
    | Cons Expr Expr              -- (:)
    | ListLit (List Expr)
    | InfixOp Name Expr Expr      --  operators +, * etc
    | IfThenElse Expr Expr Expr
    | Fail String                  -- pattern match failure, type errors, etc


type Decl
    = TypeSig Name String 
    | Equation Name (List Pattern) Expr 
    
type Pattern
    = VarP Name
    | BooleanP Bool
    | NumberP Int
    | NilP
    | ConsP Pattern Pattern
      

-- variable substitutions
type alias Subst = Dict Name Expr


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
                             
          Lam x e1 ->
              let
                  s1 = Dict.remove x s
              in
                  Lam x (applySubst s1 e1)
                      
          App e1 e2 ->
              App (applySubst s e1) (applySubst s e2)
                  
          Cons e1 e2 ->
              Cons (applySubst s e1) (applySubst s e2)
                  
          InfixOp op e1 e2 ->
              InfixOp op (applySubst s e1) (applySubst s e2)
                  
          ListLit l ->
              ListLit (List.map (applySubst s) l)
                  
          IfThenElse e1 e2 e3 ->
              IfThenElse (applySubst s e1) (applySubst s e2) (applySubst s e3)

          Fail _ -> e
