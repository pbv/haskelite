{-
  Damas Milner type checking and inference 
  Pedro Vasconcelos, 2023
-}

module Typecheck exposing (..)

import Dict exposing (Dict)
import Set
import AST exposing (Expr(..), Matching(..), Pattern(..), Bind, Name)
import Types exposing (Type(..))
import Tc exposing (Tc, pure, andThen, explain, fail)
-- import Prelude
import Pretty

-- * type environments      -
type alias TyEnv
    = Dict AST.Name Type

{-
tcProgram : List Bind -> Program -> Result String Program
tcProgram prelude prog
    = case prog of
          Letrec binds expr ->
              Tc.eval <|
              (tcRecBind (preludeEnv prelude) binds |>
               andThen (\env1 -> tcExpr env1 expr |>
                      andThen (\_ -> pure prog)))
-}      

-- typecheck a single expression      
tcExpr : TyEnv -> Expr -> Tc Type
tcExpr env expr
    = explain ("in expression " ++ Pretty.prettyExpr expr ++ ": ")
      <| case expr of
          Number _ ->
              pure TyInt
          Var v ->
              case Dict.get v env of
                  Just ty ->
                      Tc.freshInst ty
                  Nothing ->
                      fail ("undefined variable: " ++ v)

          Cons "True" [] ->
              pure TyBool
          Cons "False" [] ->
              pure TyBool
          Cons ":" [hd,tl] ->
              tcExpr env hd |>
              andThen (\tyhd ->
              tcExpr env tl |>
              andThen (\tytl ->
              Tc.unify tytl (TyList tyhd) |>
              andThen (\_ -> pure tytl)))
          Cons _ _ ->
              Tc.fail "not implemented"
              
          App fun arg ->
              tcExpr env fun |>
              andThen (\tyfun -> tcExpr env arg |>
              andThen (\tyarg -> Tc.freshVar |>
              andThen (\a -> Tc.unify tyfun (TyFun tyarg a) |>
              andThen (\_ -> pure a))))

          Lam _ match ->
              Tc.freshVar |>
              andThen (\ty -> tcMatching env match ty |>
              andThen (\_ -> pure ty))
                           

          IfThenElse e0 e1 e2 ->
              tcExpr env e0 |>
              andThen (\t0 -> Tc.unify t0 TyBool |>
              andThen (\_ -> tcExpr env e1 |>
              andThen (\t1 -> tcExpr env e2 |>
              andThen (\t2 -> Tc.unify t1 t2 |>
              andThen (\_ -> pure t1)))))

          TupleLit args ->
              Tc.traverse (tcExpr env) args |>
              andThen (\ts -> pure (TyTuple ts))

          ListLit args ->
              Tc.freshVar |>
              andThen (\a ->
              Tc.traverse (\arg -> tcExpr env arg |>
              andThen (\t -> Tc.unify t a)) args |>
              andThen (\_ -> pure (TyList a)))

          InfixOp op e1 e2 ->
              Debug.todo "infix"
              {-
              tcExpr env (Var op) |>
              andThen (\top -> tcApplication env top [e1,e2])
               -}

          Error ->
              Tc.freshVar 

extend : Name -> Type -> TyEnv -> TyEnv
extend v t env
    = Dict.insert v t env         


tcMatching : TyEnv -> Matching -> Type -> Tc ()
tcMatching env match ty
    = case match of
          Return expr _ ->
              tcExpr env expr |>
              andThen (\tyr -> Tc.unify tyr ty)
          Fail ->
              pure ()
          Match patt match1 ->
              Tc.freshVar |>
              andThen (\ty1 ->
              Tc.freshVar |>
              andThen (\ty2 ->
              Tc.unify (TyFun ty1 ty2) ty |>
              andThen (\_ ->
              tcPattern env patt ty1 |>
              andThen (\env1 ->
              tcMatching env1 match1 ty2))))

          Arg arg match1 ->
              tcExpr env arg |>
              andThen (\ty1 ->
              Tc.freshVar |>
              andThen (\ty2 ->
              Tc.unify (TyFun ty1 ty2) ty |>
              andThen (\_ ->
              tcMatching env match1 ty2)))

          Alt m1 m2 ->
              tcMatching env m1 ty |>
              andThen (\_ -> tcMatching env m2 ty)

                  
{-      
tcLambda : TyEnv -> List Name -> Expr -> Tc Type
tcLambda env vars body
    = case vars of
          [] ->
              tcExpr env body
          (v::vs) ->
              Tc.freshVar |>
              andThen (\a -> tcLambda (extend v a env) vs body |>
                           andThen (\t -> pure (TyFun a t)))
      
tcApplication : TyEnv -> Type -> List Expr -> Tc Type
tcApplication env funtype arglist
    = case arglist of
          [] ->
              pure funtype
          (arg0::rest) ->
              tcExpr env arg0 |>
              andThen (\t0 -> Tc.freshVar |>
                          andThen (\t1 -> Tc.unify funtype (TyFun t0 t1) |>
                                andThen (\_ -> tcApplication env t1 rest)))
-}


-- typechecking a pattern against a type
tcPattern : TyEnv -> Pattern -> Type -> Tc TyEnv
tcPattern env patt ty
    = case patt of
          (VarP var) -> 
              pure (extend var ty env)

          (BangP var) ->
              pure (extend var ty env)

          (ConsP "True" []) ->
              Tc.unify ty TyBool |>
              andThen (\_ -> pure env)
          (ConsP "False" []) ->
              Tc.unify ty TyBool |>
              andThen (\_ -> pure env)
          (ConsP ":" [hd,tl]) ->
              Tc.freshVar |>
              andThen (\a -> Tc.unify ty (TyList a) |>
              andThen (\_ -> tcPattern env hd a |>
              andThen (\env1 -> tcPattern env1 tl (TyList a))))
          (ConsP _ _) ->
              Tc.fail "not implemented"

          (NumberP _) ->
              Tc.unify ty TyInt |>
              andThen (\_ -> pure env)

          (ListP patts) ->
              Tc.freshVar |>
              andThen (\a -> Tc.unify ty (TyList a) |>
              andThen (\_ -> tcListPatts env patts a))


          (TupleP patts) ->
              Tc.freshVars (List.length patts) |>
              andThen (\ts -> Tc.unify ty (TyTuple ts) |>
              andThen (\_ -> tcTuplePatts env (List.map2 Tuple.pair patts ts)))



-- check many patterns against a type
tcListPatts : TyEnv -> List Pattern -> Type -> Tc TyEnv
tcListPatts env patts ty
    = case patts of
          [] ->
              pure env
          (patt::rest) ->
              tcPattern env patt ty |>
              andThen (\env1 -> tcListPatts env1 rest ty)

tcTuplePatts : TyEnv -> List (Pattern, Type) -> Tc TyEnv
tcTuplePatts env lst
    = case lst of
          [] ->
              pure env
          ((patt,ty)::rest) ->
              tcPattern env patt ty |>
              andThen (\env1 -> tcTuplePatts env1 rest)

                
{-              
-- typecheck a single alternative
tcAlt : TyEnv -> List Pattern -> Expr -> Tc Type
tcAlt env patts expr
    = case patts of
          [] ->
              tcExpr env expr
                  
          (pat::rest) ->
              Tc.freshVar |>
              andThen (\a -> tcPattern env pat a |>
                           andThen (\env1 -> tcAlt env1 rest expr |>
                                        andThen (\t -> pure (TyFun a t))))
                  
-- typecheck a list of alternatives 
tcAlts : TyEnv -> Type -> List Alt -> Tc ()
tcAlts env tr alts
    = Tc.traverse (\(ps,e) ->
                       tcAlt env ps e |>
                       andThen (\t -> Tc.unify tr t)) alts |>
      andThen (\_ -> pure ())
      





-- mutually recursive let bindings
tcRecBind : TyEnv -> List Bind -> Tc TyEnv
tcRecBind tyenv binds
    = Tc.traverse tcRecType binds |>
      andThen (\tys ->
                   let
                       names = List.map .name binds
                       tyenv1 = Dict.fromList <| List.map2 Tuple.pair names tys
                       tyenv2 = Dict.union tyenv1 tyenv
                   in
                       Tc.traverse Tc.freshInst tys |>
                       andThen (\tyrs ->
                                    let lst = List.map2 Tuple.pair binds tyrs
                                    in tcRecAlts tyenv2 lst |>
                       andThen (\_ -> tcRecGen tyenv lst )))


-- types for environment of recursive bindings
tcRecType : Bind -> Tc Type
tcRecType bind
    = case bind.typeSig of
          Nothing -> Tc.freshVar
          Just tysig -> pure tysig

                        
tcRecAlts : TyEnv -> List (Bind, Type) -> Tc ()
tcRecAlts tyenv lst
    = case lst of
          [] ->
              pure ()
          ((bind,ty) :: rest) ->
              (explain ("definition of " ++ bind.name ++ ": ") <|
                   tcAlts tyenv ty bind.alts) |>
                  andThen (\_ -> tcRecAlts tyenv rest)


tcRecGen : TyEnv -> List (Bind,Type) -> Tc TyEnv
tcRecGen tyenv lst
    = case lst of
          [] ->
              pure tyenv
          ((bind,ty) :: rest) ->
              Tc.simplify ty |>
              andThen (\ty1 ->
                           let tyinfer = Tc.generalize ty1
                           in checkTypSig bind tyinfer |>
                              andThen (\_ -> let tyenv1 = extend bind.name tyinfer tyenv
                                             in tcRecGen tyenv1 rest))

-- check user type signature if there is one                           
checkTypSig : Bind -> Type -> Tc ()
checkTypSig bind tyinfer
    = case bind.typeSig of
          Nothing ->
              pure ()
          Just tysig ->
              if tysig /= tyinfer then
                  wrongTypeSig bind.name tysig tyinfer
              else
                  pure ()

                           
wrongTypeSig : Name -> Type -> Type -> Tc a
wrongTypeSig name tysig tyinfer
     = fail ("type signature " ++ name ++ " :: " ++ Pretty.prettyType tysig
                 ++ " is too general; inferred type: " ++ Pretty.prettyType tyinfer)
-}


------------------------------------------------------------------------------
-- Prelude stuff
------------------------------------------------------------------------------
-- type environment for the Prelude
preludeEnv : List Bind -> TyEnv
preludeEnv binds
    = Dict.union primEnv <|
      List.foldl addTypeSig Dict.empty binds
                 
addTypeSig : Bind -> TyEnv -> TyEnv
addTypeSig bind tyenv
    = case bind.typeSig of
          Just ty ->
              Dict.insert bind.name (Types.generalize Set.empty ty) tyenv
          _ ->
              tyenv


-- type environment for primitives
primEnv : TyEnv
primEnv
    = let
        intOp = TyFun TyInt (TyFun TyInt TyInt)
        cmpOp = TyFun TyInt (TyFun TyInt TyBool)
      in
      Dict.fromList
      [ ("+", intOp), ("*", intOp), ("-", intOp), ("//", intOp),
        ("mod", intOp), ("div", intOp), ("negate", TyFun TyInt TyInt)
      , ("==", cmpOp), ("/=", cmpOp), ("<=", cmpOp)
      , (">=", cmpOp), ("<", cmpOp), (">", cmpOp)
      , (":", TyFun (TyGen 0) (TyFun (TyList (TyGen 0)) (TyList (TyGen 0))))
      , ("enumFrom", TyFun TyInt (TyList TyInt))
      , ("enumFromTo", TyFun TyInt (TyFun TyInt (TyList TyInt)))
      , ("enumFromThen", TyFun TyInt (TyFun TyInt (TyList TyInt)))
      , ("enumFromThenTo", TyFun TyInt (TyFun TyInt (TyFun TyInt (TyList TyInt))))
      ]
