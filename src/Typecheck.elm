{-
  Damas Milner type checking and inference 
  Pedro Vasconcelos, 2023
-}

module Typecheck exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Set
import AST exposing (Expr(..), Matching(..), Pattern(..),
                         Program(..), Module, Bind, Data(..), Name)
import Types exposing (Type(..), tyBool, tyInt, tyChar, tyString, tyOrdering)
import Tc exposing (Tc, pure, andThen, explain, fail)
import Pretty
import Heap

-- * type environments      
type alias TyEnv
    = Dict AST.Name Type

tcProgram : TyEnv -> Program  -> Result String ()
tcProgram env0 (LetProg mod expr) 
    = let
        env1 = addModuleEnv mod env0
     in
         Tc.eval <|
         (tcRecBind env1 mod.binds |>
          andThen (\env2 -> tcExpr_ env2 expr |>
                   andThen (\_ -> pure ())))


              
             
-- wrapper function that documents the expression being typechecked             
tcExpr_ : TyEnv -> Expr -> Tc Type
tcExpr_ env expr
    = explain ("in expression " ++
                   Pretty.prettyExpr Pretty.defaultOpts Heap.empty expr ++ ": ") <|
      tcExpr env expr

             
-- typecheck a single expression      
tcExpr : TyEnv -> Expr -> Tc Type
tcExpr env expr
    = case expr of
          Number _ ->
              pure tyInt
          Char _ ->
              pure tyChar
          Var v ->
              case Dict.get v env of
                  Just ty ->
                      Tc.freshInst ty
                  Nothing ->
                      fail ("undefined variable " ++ v)

          Cons tag args ->
              case Dict.get tag env of
                  Just ty ->
                      Tc.freshInst ty |>
                      andThen (\ty1 -> tcApplication env ty1 args)
                  Nothing ->
                      Tc.fail ("unknown constructor " ++ tag)
              
          App fun arg ->
              tcExpr env fun |>
              andThen (\tyfun -> tcApplication env tyfun [arg])

          Lam _ _ match ->
              Tc.freshVar |>
              andThen (\ty -> tcMatching env match ty |>
              andThen (\_ -> pure ty))
                           
          Let binds e1 ->
              tcRecBind env binds |>
              andThen (\env1 -> tcExpr env1 e1)

          Case e0 alts ->
              Tc.freshVar |>
              andThen (\ty -> tcMatching env (AST.translateCase e0 alts) ty |>
              andThen (\_ -> pure ty))
                 
              
          IfThenElse e0 e1 e2 ->
              tcExpr env e0 |>
              andThen (\t0 -> Tc.unify t0 tyBool |>
              andThen (\_ -> tcExpr env e1 |>
              andThen (\t1 -> tcExpr env e2 |>
              andThen (\t2 -> Tc.unify t1 t2 |>
              andThen (\_ -> pure t1)))))

          BinaryOp op e1 e2 ->
              tcExpr env (App (App (Var op) e1) e2)
          UnaryOp op e1 ->
              tcExpr env (App (Var op) e1)

          Error e1 ->
              tcExpr env (App (Var "error") e1)


extend : Name -> Type -> TyEnv -> TyEnv
extend v t env
    = Dict.insert v t env         


tcApplication : TyEnv -> Type -> List Expr -> Tc Type
tcApplication env tyfun args
    = case args of
          [] ->
              pure tyfun
          (e1::rest) ->
              tcExpr env e1 |>
              andThen (\tyarg -> Tc.freshVar |>
              andThen (\a -> Tc.unify tyfun (TyFun tyarg a) |>
              andThen (\_ -> tcApplication env a rest)))

tcMatching : TyEnv -> Matching -> Type -> Tc ()
tcMatching env match ty
    = case match of
          Return expr _ ->
              explain ("in expression " ++
                       Pretty.prettyExpr Pretty.defaultOpts Heap.empty expr ++ ": ")
              (tcExpr env expr |> andThen (Tc.unify ty))
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

          Arg arg m1 ->
              tcExpr env arg |>
              andThen (\ty1 ->
              tcMatching env m1 (TyFun ty1 ty))

          Alt m1 m2 ->
              tcMatching env m1 ty |>
              andThen (\_ -> tcMatching env m2 ty)

                  


-- typechecking a pattern against a type
tcPattern : TyEnv -> Pattern -> Type -> Tc TyEnv
tcPattern env patt ty
    = case patt of
          DefaultP ->
              pure env
                  
          (VarP var) -> 
              pure (extend var ty env)

          (BangP var) ->
              pure (extend var ty env)

          (ConsP tag args) ->
              case Dict.get tag env of
                  Just tyc ->
                      Tc.freshInst tyc |>
                      andThen (\tyc1 -> tcConsArgs env args tyc1 ty)
                  Nothing ->
                      Tc.fail ("unknown constructor " ++ tag)

          (NumberP _) ->
              Tc.unify ty tyInt |>
              andThen (\_ -> pure env)

          (CharP _) ->
              Tc.unify ty tyChar |>
              andThen (\_ -> pure env)



-- typecheck patterns arguments to a a constructor pattern
-- returns augmented type environment
tcConsArgs : TyEnv -> List Pattern -> Type -> Type -> Tc TyEnv
tcConsArgs env patts tyc1 ty
    = case patts of
          [] ->
              Tc.unify tyc1 ty |>
              andThen (\_ -> pure env)

          (p::ps) ->
              Tc.freshVar |>
              andThen (\a ->
              Tc.freshVar |>
              andThen (\b ->
              Tc.unify tyc1 (TyFun a b) |>
              andThen (\_ ->
              tcPattern env p a |>
              andThen (\env1 ->
              tcConsArgs env1 ps b ty))))                           
              

-- check many patterns against a type
tcListPatts : TyEnv -> List Pattern -> Type -> Tc TyEnv
tcListPatts env patts ty
    = case patts of
          [] ->
              pure env
          (patt::rest) ->
              tcPattern env patt ty |>
              andThen (\env1 -> tcListPatts env1 rest ty)

-- check patterns for a tupple                  
tcTuplePatts : TyEnv -> List (Pattern, Type) -> Tc TyEnv
tcTuplePatts env lst
    = case lst of
          [] ->
              pure env
          ((patt,ty)::rest) ->
              tcPattern env patt ty |>
              andThen (\env1 -> tcTuplePatts env1 rest)

                

tcRecBind : TyEnv -> List Bind -> Tc TyEnv
tcRecBind tyenv binds
    = Tc.traverse (tcRecType (freeTyEnvVars tyenv)) binds |>
      andThen (\tys ->
                   let
                       names = List.map .name binds
                       tyenv1 = Dict.fromList <| List.map2 Tuple.pair names tys
                       tyenv2 = Dict.union tyenv1 tyenv
                   in
                       Tc.traverse Tc.freshInst tys |>
                       andThen (\tyrs ->
                                    let
                                        lst = List.map2 Tuple.pair binds tyrs
                                    in
                                        tcRecAlts tyenv2 lst |>
                                        andThen (\_ -> tcRecGen tyenv lst)))

tcRecAlts : TyEnv -> List (Bind, Type) -> Tc ()
tcRecAlts tyenv lst
    = case lst of
          [] ->
              pure ()
          ((bind,ty) :: rest) ->
              (explain ("definition of " ++ bind.name ++ ": ") <|
               (tcExpr tyenv bind.expr |>
                andThen (\ty1 -> Tc.unify ty ty1))) |>
                  andThen (\_ -> tcRecAlts tyenv rest)



          
-- generalize recursive types 
tcRecGen : TyEnv -> List (Bind,Type) -> Tc TyEnv
tcRecGen tyenv lst
    = case lst of
          [] ->
              pure tyenv
          ((bind,ty) :: rest) ->
              Tc.simplify ty |>
              andThen (\ty1 ->
                           let
                               ftvs = freeTyEnvVars tyenv
                               tyinfer = Types.generalize ftvs ty1
                           in
                               checkTypSig bind tyinfer |>
                               andThen (\_ ->
                                            let tyenv1 = extend bind.name tyinfer tyenv
                                             in tcRecGen tyenv1 rest))


          
-- get a type for a binding
tcRecType : Set Name -> Bind -> Tc Type
tcRecType ftvs bind
    = case bind.typeSig of
          Nothing ->
              Tc.freshVar
          Just tysig ->
              let tysig1 = Types.generalize ftvs tysig
              in pure tysig1


-- check annotated type signature against infered type
checkTypSig : Bind -> Type -> Tc ()
checkTypSig bind tyinfer
    = case bind.typeSig of
          Nothing ->
              pure ()
          Just tysig ->
              if Types.generalize Set.empty tysig /= tyinfer then
                  fail ("type signature " ++ bind.name ++ " :: " ++ 
                        Pretty.prettyType tysig ++ 
                        " is too general; inferred type: " ++
                        Pretty.prettyType tyinfer)
              else
                  pure ()


-- free type vars in a typing environment
freeTyEnvVars : TyEnv -> Set Name
freeTyEnvVars 
    = Dict.foldl (\_ ty acc -> Set.union (Types.freeTyVars ty) acc) Set.empty 
                      

------------------------------------------------------------------------------
-- Handling type environments
------------------------------------------------------------------------------
addModuleEnv : Module -> TyEnv -> TyEnv
addModuleEnv mod tyenv
    = addBindings mod.binds (addDataDecls mod.dataDecls tyenv)

-- extend a type environment with a list of bindings
addBindings : List Bind -> TyEnv -> TyEnv
addBindings binds tyenv
    = List.foldl addTypeSig tyenv binds
                 
addTypeSig : Bind -> TyEnv -> TyEnv
addTypeSig bind tyenv
    = case bind.typeSig of
          Just ty ->
              Dict.insert bind.name (Types.generalize Set.empty ty) tyenv
          _ ->
              tyenv

-- extend a type environment with a list of data declarations
addDataDecls : List Data -> TyEnv -> TyEnv
addDataDecls ddecls env
    = case ddecls of
          [] ->
              env
          (decl :: rest) ->
              addDataDecls rest (addDataDecl decl env)

addDataDecl : Data -> TyEnv -> TyEnv
addDataDecl (Data _ alts) env
    = let env1 =
              Dict.fromList
                  (List.map (\(con,ty) -> (con, Types.generalize Set.empty ty))
                       alts)
      in
          Dict.union env env1


------------------------------------------------------------------------------

-- initial typing environment for primitives
initialEnv : TyEnv
initialEnv
    = let
        intOp = TyFun tyInt (TyFun tyInt tyInt)
        -- NB: no typeclasses so these types are overly polymorphic!
        a = TyGen 0
        b = TyGen 1
        cmpOp = TyFun a (TyFun a tyBool)
        orderOp = TyFun a (TyFun a tyOrdering)
      in
      Dict.fromList
      [ ("+", intOp), ("*", intOp), ("-", intOp), 
        ("mod", intOp), ("div", intOp), ("negate", TyFun tyInt tyInt)
      , ("==", cmpOp), ("/=", cmpOp), ("<=", cmpOp)
      , (">=", cmpOp), ("<", cmpOp), (">", cmpOp)
      , ("compare", orderOp)
      , ("error", TyFun tyString a)
      , (":", TyFun a (TyFun (TyList a) (TyList a)))
      , ("[]", TyList a)
      -- TODO: generalize this for more tuples 
      , (",", TyFun a (TyFun b (TyTuple [a, b])))
      , ("enumFrom", TyFun tyInt (TyList tyInt))
      , ("enumFromTo", TyFun tyInt (TyFun tyInt (TyList tyInt)))
      , ("enumFromThen", TyFun tyInt (TyFun tyInt (TyList tyInt)))
      , ("enumFromThenTo", TyFun tyInt (TyFun tyInt (TyFun tyInt (TyList tyInt))))
      , ("chr", TyFun tyInt tyChar)
      , ("ord", TyFun tyChar tyInt)
      , ("toUpper", TyFun tyChar tyChar)
      , ("toLower", TyFun tyChar tyChar)
      , ("isUpper", TyFun tyChar tyBool)
      , ("isLower", TyFun tyChar tyBool)
      , ("isDigit", TyFun tyChar tyBool)
      , ("isAlpha", TyFun tyChar tyBool)
      , ("isAlphaNum", TyFun tyChar tyBool)
      ]
