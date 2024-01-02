{-
  Damas Milner type checking and inference 
  Pedro Vasconcelos, 2023
-}

module Typecheck exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Set
import AST exposing (Expr(..), Matching(..), Pattern(..),
                      Program(..), Module, Bind, Decl(..), DataDecl, Name, Tag)
import Types exposing (Type(..), Kind(..), Tycon, Tyvar,
                       tyBool, tyInt, tyChar, tyString, tyOrdering)
import Tc exposing (Tc, pure, andThen, traverse, traverse_, explain, fail)
import Pretty
import Machine.Heap as Heap

-- * type environments      
type alias TyEnv
    = Dict Name Type

-- * kind environments
-- only for type constructors 
-- no higher-kinded type variables 
type alias KindEnv
    = Dict Tycon Kind
      
tcMain : KindEnv -> TyEnv -> Program -> Result String ()
tcMain kenv tenv prog
    = Tc.eval <| (tcProgram kenv tenv prog |> andThen (\_ -> pure ()))

-- type a program: check a module and an expression
tcProgram : KindEnv -> TyEnv -> Program -> Tc Type
tcProgram kenv tenv (LetProg mod expr) 
    = tcModule kenv tenv mod |>
      andThen (\(_, tenv1) -> tcExpr_ tenv1 expr)

                      
-------------------------------------------------------------------------
-- typechecking modules
------------------------------------------------------------------------
tcModule : KindEnv -> TyEnv -> Module -> Tc (KindEnv, TyEnv)
tcModule kenv0 tenv0 mod
    = let kenv1 = getKindEnv kenv0 mod.dataDecls
          tenv1 = getModuleEnv mod tenv0         
      in wellformedDataDecls kenv1 mod.dataDecls |>
         andThen (\_ -> wellformedBinds kenv1 mod.binds |>
         andThen (\_ -> tcRecBinds tenv1 mod.binds |>
                           (andThen (\tenv2 -> pure (kenv1,tenv2)))))
         

-- given a result data type, find the kind of the type constructor
getKindDecl : Type -> Maybe (Tycon, Kind)
getKindDecl ty 
    = case ty of
          TyConst c ts ->
              let kind = List.foldr (\_ k -> KindFun KindStar k) KindStar ts
              in Just (c, kind)
          _ ->
              Nothing

-- augment a kind environment with a list of data declararations
getKindEnv : KindEnv -> List DataDecl -> KindEnv
getKindEnv kindenv ddecls
    = List.foldl (\(c,k) e -> Dict.insert c k e) kindenv <|
      List.filterMap (.result >> getKindDecl) ddecls
          
wellformedDataDecls : KindEnv -> List DataDecl -> Tc ()
wellformedDataDecls kindenv ddecls
    = traverse_ (.alternatives >> wellformedAlternatives kindenv) ddecls

-- check that type signatures are well-kinded
wellformedBinds : KindEnv -> List Bind -> Tc ()
wellformedBinds kindenv binds
    = traverse_ (wellformedSignature kindenv) binds
      
wellformedSignature : KindEnv -> Bind -> Tc ()
wellformedSignature kindenv bind
    = case bind.typeSig of
          Just typ ->
              explain ("type signature for "++ bind.name ++ ", ")
                  (wellformedType kindenv typ)
          Nothing ->
              pure ()

                  
-- check that all alternatives in a data declaration are well-kinded
wellformedAlternatives : KindEnv -> List (Tag, Type) -> Tc ()
wellformedAlternatives kindenv alts
    = traverse_ (\(cons, typ) -> explain ("in data constructor " ++ cons ++ ", ")
                                   (wellformedType kindenv typ)) alts
 

-- check if a type is well-formed wrt a given kind environment
wellformedType : KindEnv -> Type -> Tc ()
wellformedType kindenv ty
    = case ty of
          TyVar _ ->  -- always at kind *
              pure ()
          TyGen _ ->
              pure ()
          TyList t ->
              wellformedType kindenv t
          TyTuple ts ->
              wellformedTypes kindenv ts
          TyFun t1 t2 ->
              wellformedTypes kindenv [t1,t2]
          TyConst c ts ->
              case Dict.get c kindenv of
                  Just kind ->
                      wellformedTypes kindenv ts |>
                      andThen (\_ -> explain ("in type constructor " ++ c ++ ": ")
                                     (wellformedApp kind ts))
                  Nothing ->
                      fail ("unknown type constructor: " ++ c)

wellformedApp : Kind -> List Type -> Tc ()
wellformedApp kind tys
    = case tys of
          [] ->
              if kind == KindStar then
                  pure ()
              else
                  fail "missing type argument"

          (ty :: rest) ->
              case kind of
                  KindFun KindStar kresult ->
                      wellformedApp kresult rest
                  _ ->
                      fail "too many type arguments"
                      
wellformedTypes : KindEnv -> List Type -> Tc ()
wellformedTypes kindenv ts
    = traverse_ (wellformedType kindenv) ts

                  
      
-----------------------------------------------------------------------
-- typechecking expressions             
-----------------------------------------------------------------------          
             
-- wrapper function that documents the expression being typechecked             
tcExpr_ : TyEnv -> Expr -> Tc Type
tcExpr_ tenv expr
    = explain ("in expression " ++
                  Pretty.prettyExpr Pretty.defaultOpts Heap.empty expr ++ ": ") <|
      tcExpr tenv expr

             
-- typecheck a single expression      
tcExpr : TyEnv -> Expr -> Tc Type
tcExpr tenv expr
    = case expr of
          Number _ ->
              pure tyInt
          Char _ ->
              pure tyChar
          Var v ->
              case Dict.get v tenv of
                  Just ty ->
                      Tc.freshInst ty
                  Nothing ->
                      fail ("undefined variable: " ++ v)

          Cons tag args ->
              case Dict.get tag tenv of
                  Just ty ->
                      Tc.freshInst ty |>
                      andThen (\ty1 -> tcApplication tenv ty1 args)
                  Nothing ->
                      Tc.fail ("unknown constructor " ++ tag)
              
          App fun arg ->
              tcExpr tenv fun |>
              andThen (\tyfun -> tcApplication tenv tyfun [arg])

          Lam _ _ match ->
              Tc.freshType |>
              andThen (\ty -> tcMatching tenv match ty |>
              andThen (\_ -> pure ty))
                           
          Let binds e1 ->
              tcRecBinds tenv binds |>
              andThen (\tenv1 -> tcExpr tenv1 e1)

          Case e0 alts ->
              Tc.freshType |>
              andThen (\ty -> tcMatching tenv (translateCase e0 alts) ty |>
              andThen (\_ -> pure ty))
                 
              
          IfThenElse e0 e1 e2 ->
              tcExpr tenv e0 |>
              andThen (\t0 -> Tc.unify t0 tyBool |>
              andThen (\_ -> tcExpr tenv e1 |>
              andThen (\t1 -> tcExpr tenv e2 |>
              andThen (\t2 -> Tc.unify t1 t2 |>
              andThen (\_ -> pure t1)))))

          BinaryOp op e1 e2 ->
              tcExpr tenv (App (App (Var op) e1) e2)
          UnaryOp op e1 ->
              tcExpr tenv (App (Var op) e1)

          Error e1 ->
              tcExpr tenv (App (Var "error") e1)


extend : Name -> Type -> TyEnv -> TyEnv
extend v t tenv
    = Dict.insert v t tenv         


tcApplication : TyEnv -> Type -> List Expr -> Tc Type
tcApplication tenv tyfun args
    = case args of
          [] ->
              pure tyfun
          (e1::rest) ->
              tcExpr tenv e1 |>
              andThen (\tyarg -> Tc.freshType |>
              andThen (\a -> Tc.unify tyfun (TyFun tyarg a) |>
              andThen (\_ -> tcApplication tenv a rest)))

tcMatching : TyEnv -> Matching -> Type -> Tc ()
tcMatching tenv match ty
    = case match of
          Return expr _ ->
              explain ("in expression " ++
                       Pretty.prettyExpr Pretty.defaultOpts Heap.empty expr ++ ": ")
              (tcExpr tenv expr |> andThen (Tc.unify ty))
          Fail ->
              pure ()
          Match patt match1 ->
              Tc.freshType |>
              andThen (\ty1 ->
              Tc.freshType |>
              andThen (\ty2 ->
              Tc.unify (TyFun ty1 ty2) ty |>
              andThen (\_ ->
              tcPattern tenv patt ty1 |>
              andThen (\tenv1 ->
              tcMatching tenv1 match1 ty2))))

          Arg arg m1 ->
              tcExpr tenv arg |>
              andThen (\ty1 ->
              tcMatching tenv m1 (TyFun ty1 ty))

          Alt m1 m2 ->
              tcMatching tenv m1 ty |>
              andThen (\_ -> tcMatching tenv m2 ty)

          Where binds m2 ->
              tcRecBinds tenv binds |>
              andThen  (\tenv1 -> tcMatching tenv1 m2 ty)

                  


-- typechecking a pattern against a type
tcPattern : TyEnv -> Pattern -> Type -> Tc TyEnv
tcPattern tenv patt ty
    = case patt of
          DefaultP ->
              pure tenv
                  
          (VarP var) -> 
              pure (extend var ty tenv)

          (BangP var) ->
              pure (extend var ty tenv)

          (ConsP tag args) ->
              case Dict.get tag tenv of
                  Just tyc ->
                      Tc.freshInst tyc |>
                      andThen (\tyc1 -> tcConsArgs tenv args tyc1 ty)
                  Nothing ->
                      Tc.fail ("unknown constructor: " ++ tag)

          (NumberP _) ->
              Tc.unify ty tyInt |>
              andThen (\_ -> pure tenv)

          (CharP _) ->
              Tc.unify ty tyChar |>
              andThen (\_ -> pure tenv)



-- typecheck patterns arguments to a a constructor pattern
-- returns augmented type environment
tcConsArgs : TyEnv -> List Pattern -> Type -> Type -> Tc TyEnv
tcConsArgs tenv patts tyc1 ty
    = case patts of
          [] ->
              Tc.unify tyc1 ty |>
              andThen (\_ -> pure tenv)

          (p::ps) ->
              Tc.freshType |>
              andThen (\a ->
              Tc.freshType |>
              andThen (\b ->
              Tc.unify tyc1 (TyFun a b) |>
              andThen (\_ ->
              tcPattern tenv p a |>
              andThen (\tenv1 ->
              tcConsArgs tenv1 ps b ty))))                           
              

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

                
-- typecheck a list of bindings
tcRecBinds : TyEnv -> List Bind -> Tc TyEnv
tcRecBinds tyenv binds
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
                               checkTypSig ftvs bind tyinfer |>
                               andThen (\_ ->
                                            let tyenv1 = extend bind.name tyinfer tyenv
                                             in tcRecGen tyenv1 rest))


          
-- get a type for a binding
tcRecType : Set Name -> Bind -> Tc Type
tcRecType ftvs bind
    = case bind.typeSig of
          Nothing ->
              Tc.freshType
          Just tysig ->
              let tysig1 = Types.generalize ftvs tysig
              in pure tysig1


-- check annotated type signature against infered type
checkTypSig : Set Name -> Bind -> Type -> Tc ()
checkTypSig ftvs bind tyinfer
    = case bind.typeSig of
          Nothing ->
              pure ()
          Just tysig ->
              let gtysig = Types.generalize ftvs tysig
              in
              if gtysig /= tyinfer then
                  fail ("type signature " ++ bind.name ++ " :: " ++ 
                        Pretty.prettyType tysig ++ 
                        " is too general; inferred type: " ++
                        Pretty.prettyType tyinfer)
              else
                  pure ()


-- free type vars in a typing environment
freeTyEnvVars : TyEnv -> Set Tyvar
freeTyEnvVars tyenv
    = Set.fromList <| Dict.foldl (\_ ty acc -> Types.freeTyVars ty ++ acc) [] tyenv

--    = Dict.foldl (\_ ty acc -> Set.union (Set.fromList <| Types.freeTyVars ty) acc) Set.empty 
                      

------------------------------------------------------------------------------
-- Handling type environments
------------------------------------------------------------------------------
getModuleEnv : Module -> TyEnv -> TyEnv
getModuleEnv mod tyenv
    = getBindsEnv mod.binds (getDataEnv mod.dataDecls tyenv)

-- extend a type environment with a list of bindings
getBindsEnv : List Bind -> TyEnv -> TyEnv
getBindsEnv binds tyenv
    = List.foldl getTypeSig tyenv binds
                 
getTypeSig : Bind -> TyEnv -> TyEnv
getTypeSig bind tyenv
    = case bind.typeSig of
          Just ty ->
              Dict.insert bind.name (Types.generalize Set.empty ty) tyenv
          _ ->
              tyenv

-- extend a type environment with a list of data declarations
getDataEnv : List DataDecl -> TyEnv -> TyEnv
getDataEnv ddecls env
    = List.foldl getDataDecl env ddecls


getDataDecl : DataDecl -> TyEnv -> TyEnv
getDataDecl ddecl env 
    = List.foldl
      (\(con,ty) acc -> Dict.insert con (Types.generalize Set.empty ty) acc)
      env ddecl.alternatives


-- syntax translations
translateCase : Expr -> List (Pattern,Expr) -> Matching
translateCase e0 alts
    = let
        body = List.foldr
                 (\(patt,expr) rest ->
                      Alt (Match patt (Return expr Nothing)) rest)
                   Fail alts
      in Arg e0 body

          
             
------------------------------------------------------------------------------
-- initial kind environment for primitives
initialKindEnv : KindEnv
initialKindEnv
    = Dict.fromList
      [ ("Int", KindStar), ("Char", KindStar) ]

-- initial typing environment for primitives
initialTypeEnv : TyEnv
initialTypeEnv
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
