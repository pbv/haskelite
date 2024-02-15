{-
  Type checking and inference for Haskelite
  Pedro Vasconcelos, 2023
-}

module Typecheck exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Set
import AST exposing (Expr(..), Matching(..), Pattern(..),
                      Program(..), Module, Bind, Decl(..),
                         AliasDecl, DataDecl, Name, Tag)
import Types exposing (Type(..), Kind(..), Tycon, Tyvar, 
                       tyBool, tyInt, tyChar, tyString, tyList,
                           tyPair, tyTuple3, tyOrdering, applyTySubst)
import Tc exposing (Tc, pure, andThen, traverse, traverse_, explain, fail)
import Shows 

-- * type environments for term variables and term constructors  
type alias TyEnv
    = Dict String Type

-- * kind environments for type variables and type constructors
-- all type variables are of kind * (no higher-kinded types)
type alias KindEnv
    = Dict String Kind

-- * type aliases expansions
type alias TyAliases
    = Dict Tycon (List Tyvar, Type)

      
tcMain : KindEnv -> TyEnv -> Program -> Result String ()
tcMain kenv tenv prog
    = Tc.eval <| (tcProgram kenv tenv prog |> andThen (\_ -> pure ()))

-- type a program: check a module and an expression
tcProgram : KindEnv -> TyEnv -> Program -> Tc Type
tcProgram kenv tenv (LetProg mod expr) 
    = tcModule kenv tenv mod |>
      andThen (\(kenv1, tenv1) -> tcExprWrap kenv1 tenv1 expr)

                      
-------------------------------------------------------------------------
-- typechecking modules
------------------------------------------------------------------------
-- 1) extend the kind environment with the type constructors
-- for data and synonym declarations
-- 2) check and expand type synonyms
-- 3) continue typechecking the de-sugared module
tcModule : KindEnv -> TyEnv -> Module -> Tc (KindEnv, TyEnv)
tcModule kenv tenv mod
    = let kenv1 = extendKindEnv kenv <|
                  (List.map getKindDecl mod.aliasDecls ++
                   List.map getKindDecl mod.dataDecls)
      in tcExpandModule kenv1 mod |>
         andThen (\mod1 -> tcModule1 kenv1 tenv mod1)


-- type check a desugared module
-- here we can assume that type synonyms have been checked and expanded
tcModule1 : KindEnv -> TyEnv -> Module -> Tc (KindEnv, TyEnv)
tcModule1 kenv tenv mod
    = if List.isEmpty mod.aliasDecls then
              tcDataDecls kenv tenv mod.dataDecls |>
              andThen (\tenv1 -> wellformedBinds kenv mod.binds |>
              andThen (\_ -> tcRecBinds kenv tenv1 mod.binds |>
              andThen (\tenv2 -> pure (kenv,tenv2))))
      else
          fail "type synonym declarations should have been expanded"

          

-- typecheck data declarations;
-- returns an extended typing environment with the data constructors
tcDataDecls : KindEnv -> TyEnv -> List DataDecl -> Tc TyEnv
tcDataDecls kenv tenv ddecls
    = case ddecls of
          [] ->
              pure tenv
          (ddecl::rest) ->
              tcDataDecl kenv tenv ddecl |>
              andThen (\tenv1 -> tcDataDecls kenv tenv1 rest)

tcDataDecl : KindEnv -> TyEnv -> DataDecl -> Tc TyEnv
tcDataDecl kenv tyenv ddecl
    = let kenv1 = extendKindEnv kenv <| List.map (\v -> (v,KindStar)) ddecl.args
      in
          explain ("declaration for " ++ Shows.quote ddecl.tycon ++ ", ") <|
            (wellformedAlternatives kenv1 ddecl.alternatives |>
             andThen (\_ -> pure (extendDataDecl ddecl tyenv)))

      
-- check that type signatures for bindings are well-kinded
wellformedBinds : KindEnv -> List Bind -> Tc ()
wellformedBinds kenv binds
    = traverse_ (wellformedSignature kenv) binds
   
wellformedSignature : KindEnv -> Bind -> Tc ()
wellformedSignature kenv bind
    = case bind.typeSig of
          Just typ ->
              let
                  tvs = Types.freeTyVars typ
                  kenv1 = extendKindEnv kenv <| List.map (\v -> (v,KindStar)) tvs
              in 
                  explain ("type signature for "++ bind.name ++ ", ")
                      (wellformedType kenv1 typ)
          Nothing ->
              pure ()

                  
-- check that all alternatives in a data declaration are well-kinded
wellformedAlternatives : KindEnv -> List (Tag, Type) -> Tc ()
wellformedAlternatives kenv alts
    = traverse_ (\(cons, typ) ->
                     explain ("in constructor " ++ Shows.quote cons ++ ", ")
                                   (wellformedType kenv typ)) alts
 

-- check if a type is well-formed wrt a given kind environment
wellformedType : KindEnv -> Type -> Tc ()
wellformedType kenv ty
    = case ty of
          TyVar v ->  
              case Dict.get v kenv of
                  Just _ ->
                      pure ()
                  Nothing ->
                      fail ("unbound type variable " ++ Shows.quote v)
          TyGen _ ->
              pure ()
          TyFun t1 t2 ->
              wellformedTypes kenv [t1,t2]
          TyConst c ts ->
              case Dict.get c kenv of
                  Just kind ->
                      wellformedTypes kenv ts |>
                      andThen (\_ -> explain ("in type constructor " ++ Shows.quote c ++ ": ")
                                     (wellformedApp kind ts))
                  Nothing ->
                      fail ("unknown type constructor " ++ Shows.quote c)

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
wellformedTypes kenv ts
    = traverse_ (wellformedType kenv) ts

                  
------------------------------------------------------------------------------
-- check and expand type synonyms in a module
-------------------------------------------------------------------------------
tcExpandModule : KindEnv -> Module -> Tc Module
tcExpandModule kenv mod
    = let tyalias = makeAliases mod.aliasDecls
      in
          wellformedAliasDecls kenv tyalias mod.aliasDecls |>
          andThen (\_ -> tcExpandDataDecls tyalias mod.dataDecls |>
          andThen (\ddecls -> tcExpandBinds tyalias mod.binds |>
          andThen (\binds -> pure { dataDecls = ddecls,
                                    binds = binds,
                                    aliasDecls = [] })))

wellformedAliasDecls : KindEnv -> TyAliases -> List AliasDecl -> Tc ()
wellformedAliasDecls kenv tyalias adecls
    = traverse_ (wellformedAliasDecl kenv tyalias) adecls

-- check for cycles in expansion and wellformed types 
wellformedAliasDecl : KindEnv -> TyAliases -> AliasDecl -> Tc ()
wellformedAliasDecl kenv tyalias adecl
    = let
        kenv1 = extendKindEnv kenv <| List.map (\v->(v,KindStar)) adecl.args
      in
        explain ("in declaration for type synonym " ++
                     Shows.quote adecl.tycon ++ ": ") 
            (expand tyalias adecl.tyexp |>
             andThen (\_ -> wellformedType kenv1 adecl.tyexp))


makeAliases : List AliasDecl -> TyAliases
makeAliases adecls
    = Dict.fromList <| List.map (\d -> (d.tycon, (d.args,d.tyexp))) adecls


tcExpandDataDecls : TyAliases -> List DataDecl -> Tc (List DataDecl)
tcExpandDataDecls tyalias ddecls
    = traverse (tcExpandDataDecl tyalias) ddecls

tcExpandDataDecl : TyAliases -> DataDecl -> Tc DataDecl
tcExpandDataDecl tyalias ddecl
    = explain ("in declaration for " ++ Shows.quote ddecl.tycon ++ ", ") <|
      (traverse (\(tag, ty) -> expand tyalias ty |> andThen (\ty1 -> pure (tag,ty1))) ddecl.alternatives |>
      andThen (\alts -> pure {ddecl | alternatives=alts}))



          
tcExpandBinds : TyAliases -> List Bind -> Tc (List Bind)
tcExpandBinds tyalias binds
    = traverse (tcExpandBind tyalias) binds

tcExpandBind : TyAliases -> Bind -> Tc Bind
tcExpandBind tyalias bind
    = case bind.typeSig of
          Nothing ->
              pure bind
          Just ty ->
              expand tyalias ty |>
              andThen (\ty1 -> pure { bind | typeSig = Just ty1 })
                  
      
tcExpandAliases : TyAliases -> List AliasDecl -> Tc (List AliasDecl)
tcExpandAliases tyalias adecls
    = traverse (tcExpandAlias tyalias) adecls
      
tcExpandAlias tyalias adecl
    = expand tyalias adecl.tyexp |>  andThen (\ty1 -> pure {adecl|tyexp=ty1})
      

-- type expansion using synonyms 
expand : TyAliases -> Type -> Tc Type
expand tyalias ty
    = case expandRec tyalias [] ty of
          Err msg ->
              fail msg
          Ok ty1 ->
              pure ty1

-- worker function
-- 2nd argument is the list of previously expanded type constructors
-- (used to detect cyclic definitions)
expandRec : TyAliases -> List Tycon -> Type -> Result String Type
expandRec tyalias acc ty
    = case ty of
          TyVar _ ->
              Ok ty
          TyGen _ ->
              Ok ty
          TyFun t1 t2 ->
              expandRec tyalias acc t1 |>
              Result.andThen (\t1e ->
                       expandRec tyalias acc t2 |>
                           Result.andThen (\t2e -> Ok (TyFun t1e t2e)))
          TyConst c ts ->
              case Dict.get c tyalias of
                  Nothing ->
                      expandList tyalias acc ts |>
                      Result.andThen (\ts1 -> Ok (TyConst c ts1))
                  Just (vs, ty1) ->
                      if List.member c acc then
                          Err ("cycle in type synonym declaration: " ++
                                   showCycle (c::acc))
                      else
                          if List.length vs /= List.length ts then 
                              Err ("wrong number of arguments to type synonym "++
                                      Shows.quote c)
                          else
                              let s = Dict.fromList <| List.map2 Tuple.pair vs ts
                              in expandRec tyalias (c::acc) (applyTySubst s ty1)

showCycle : List String -> String
showCycle cs
    = String.concat <| List.intersperse " -> " (List.reverse cs)
                                  
                                  
expandList : TyAliases -> List Tycon -> List Type -> Result String (List Type)
expandList tyalias acc ts
    = case ts of
          [] ->
              Ok []
          (first::rest) ->
              expandRec tyalias acc first |>
              Result.andThen (\first1 -> expandList tyalias acc rest |>
                                  Result.andThen (\rest1 -> Ok (first1::rest1)))
      
      
-----------------------------------------------------------------------
-- typechecking expressions             
-----------------------------------------------------------------------          
             
-- wrapper function that documents the expression being typechecked             
tcExprWrap : KindEnv -> TyEnv -> Expr -> Tc Type
tcExprWrap kenv tenv expr
    = explain ("in expression " ++ Shows.showExpr expr ++ ": ") <|
      tcExpr kenv tenv expr

             
-- typecheck a single expression      
tcExpr : KindEnv -> TyEnv -> Expr -> Tc Type
tcExpr kenv tenv expr
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
                      fail ("undefined variable: " ++ Shows.quote v)
                          
          Cons _ tag args ->
              case Dict.get tag tenv of
                  Just ty ->
                      Tc.freshInst ty |>
                      andThen (\ty1 -> tcApplication kenv tenv ty1 args)
                  Nothing ->
                      Tc.fail ("unknown constructor: " ++ Shows.quote tag)
              
          App fun arg ->
              tcExpr kenv tenv fun |>
              andThen (\tyfun -> tcApplication kenv tenv tyfun [arg])

          Lam _ _ match ->
              Tc.freshType |>
              andThen (\ty -> tcMatching kenv tenv match ty |>
              andThen (\_ -> pure ty))
                           
          Let binds e1 ->
              wellformedBinds kenv binds |>
              andThen (\_ -> tcRecBinds kenv tenv binds |>
              andThen (\tenv1 -> tcExpr kenv tenv1 e1))

          Case e0 alts ->
              Tc.freshType |>
              andThen (\ty -> tcMatching kenv tenv (translateCase e0 alts) ty |>
              andThen (\_ -> pure ty))
                 
              
          IfThenElse e0 e1 e2 ->
              tcExpr kenv tenv e0 |>
              andThen (\t0 -> unify t0 tyBool |>
              andThen (\_ -> tcExpr kenv tenv e1 |>
              andThen (\t1 -> tcExpr kenv tenv e2 |>
              andThen (\t2 -> unify t1 t2 |>
              andThen (\_ -> pure t1)))))

          BinaryOp op e1 e2 ->
              tcExpr kenv tenv (App (App (Var op) e1) e2)
                  
          UnaryOp op e1 ->
              tcExpr kenv tenv (App (Var op) e1)

          Exception _ ->
              Tc.freshType                 -- error admits any type



extend : Name -> Type -> TyEnv -> TyEnv
extend v t tenv
    = Dict.insert v t tenv         

extendTyEnv : TyEnv -> List (Name, Type) -> TyEnv
extendTyEnv tenv pairs
    = List.foldr (\(v,t) e -> Dict.insert v t e) tenv pairs
      

tcApplication : KindEnv -> TyEnv -> Type -> List Expr -> Tc Type
tcApplication kenv tenv tyfun args
    = case args of
          [] ->
              pure tyfun
          (e1::rest) ->
              tcExpr kenv tenv e1 |>
              andThen (\tyarg -> Tc.freshType |>
              andThen (\a -> unify tyfun (TyFun tyarg a) |>
              andThen (\_ -> tcApplication kenv tenv a rest)))


-- * typechecking a matching
tcMatching : KindEnv -> TyEnv -> Matching -> Type -> Tc ()
tcMatching kenv tenv match ty
    = case match of
          Return expr _ ->
              explain ("in expression " ++ Shows.showExpr expr ++ ": ")
              (tcExpr kenv tenv expr |>
               andThen (unify ty))
                  
          Fail ->
              pure ()
                  
          Match patt match1 ->
              Tc.freshType |>
              andThen (\ty1 ->
              Tc.freshType |>
              andThen (\ty2 ->
              unify (TyFun ty1 ty2) ty |>
              andThen (\_ ->
              tcPattern tenv patt ty1 |>
              andThen (\tenv1 ->
              tcMatching kenv tenv1 match1 ty2))))

          Arg arg m1 ->
              tcExpr kenv tenv arg |>
              andThen (\ty1 ->
              tcMatching kenv tenv m1 (TyFun ty1 ty))

          Alt m1 m2 ->
              tcMatching kenv tenv m1 ty |>
              andThen (\_ -> tcMatching kenv tenv m2 ty)

          Where binds m2 ->
              wellformedBinds kenv binds |>
              andThen (\_ -> tcRecBinds kenv tenv binds |>
              andThen (\tenv1 -> tcMatching kenv tenv1 m2 ty))

                  


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
                      andThen (\tyc1 -> explain ("in pattern " ++
                                                     Shows.showPattern patt ++ ": ")
                                          <| tcConsArgs tenv args tyc1 ty)
                  Nothing ->
                      Tc.fail ("unknown constructor " ++ Shows.quote tag)

          (NumberP _) ->
              unify ty tyInt |>
              andThen (\_ -> pure tenv)

          (CharP _) ->
              unify ty tyChar |>
              andThen (\_ -> pure tenv)

          (AsP var patt1) ->
              let tenv1 = extend var ty tenv
              in tcPattern tenv1 patt1 ty 



-- typecheck patterns arguments to a a constructor pattern
-- returns augmented type environment
tcConsArgs : TyEnv -> List Pattern -> Type -> Type -> Tc TyEnv
tcConsArgs tenv patts tyc1 ty
    = case patts of
          [] ->
              unify tyc1 ty |>
              andThen (\_ -> tcConsCheck ty |> andThen (\_ -> pure tenv))
          (p::ps) ->
              Tc.freshType |>
              andThen (\a ->
              Tc.freshType |>
              andThen (\b ->
              unify tyc1 (TyFun a b) |>
              andThen (\_ ->
              tcPattern tenv p a |>
              andThen (\tenv1 ->
              tcConsArgs tenv1 ps b ty))))                           

-- check that a constructor pattern was fully applied
tcConsCheck : Type -> Tc ()
tcConsCheck ty
    = Tc.simplify ty |>
      andThen (\ty1 ->
                   case ty1 of
                       TyFun _ _ ->
                           fail "wrong number of arguments"
                       _ ->
                           pure ()
              )
              

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
tcRecBinds : KindEnv -> TyEnv -> List Bind -> Tc TyEnv
tcRecBinds kenv tyenv binds
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
                                        tcRecAlts kenv tyenv2 lst |>
                                        andThen (\_ -> tcRecGen tyenv lst)))

tcRecAlts : KindEnv -> TyEnv -> List (Bind, Type) -> Tc ()
tcRecAlts kenv tyenv lst
    = case lst of
          [] ->
              pure ()
          ((bind,ty) :: rest) ->
              (explain ("definition of " ++ bind.name ++ ": ") <|
               (tcExpr kenv tyenv bind.expr |>
                andThen (\ty1 -> unify ty ty1))) |>
                  andThen (\_ -> tcRecAlts kenv tyenv rest)



          
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
                        Shows.showType tysig ++ 
                        " is too general; inferred type: " ++
                        Shows.showType tyinfer)
              else
                  pure ()


-- free type vars in a typing environment
freeTyEnvVars : TyEnv -> Set Tyvar
freeTyEnvVars tyenv
    = Set.fromList <| Dict.foldl (\_ ty acc -> Types.freeTyVars ty ++ acc) [] tyenv



-------------------------------------------------------------------------------
-- Unification wrapper
-------------------------------------------------------------------------------
unify : Type -> Type -> Tc ()
unify = Tc.unify Shows.showType
      
------------------------------------------------------------------------------
-- Handling type and kind environments
------------------------------------------------------------------------------
-- extend a kind environment
extendKindEnv : KindEnv -> List (String,Kind) -> KindEnv
extendKindEnv kindenv pairs
    = List.foldl (\(v,k) e -> Dict.insert v k e) kindenv pairs
    
-- get the pair constructor and kind from a data or type declaration
getKindDecl : {r | tycon:Tycon, args:List Tyvar} -> (Tycon, Kind)
getKindDecl decl
    = let kind = List.foldr (\_ k -> KindFun KindStar k) KindStar decl.args
      in (decl.tycon, kind)

-- extend a type environment with type signatures from bindings
extendBindsEnv : List Bind -> TyEnv -> TyEnv
extendBindsEnv binds tyenv
    = extendTyEnv tyenv <| List.filterMap getAssignedType binds
      
getAssignedType : Bind -> Maybe (Name, Type)
getAssignedType bind
    = Maybe.map (\ty -> (bind.name, Types.generalize Set.empty ty)) bind.typeSig
    
-- extend a type environment with a list of data declarations
extendDataEnv : List DataDecl -> TyEnv -> TyEnv
extendDataEnv ddecls env
    = List.foldl extendDataDecl env ddecls

extendDataDecl : DataDecl -> TyEnv -> TyEnv
extendDataDecl ddecl env 
    = extendTyEnv env <| getDataTypes ddecl

getDataTypes : DataDecl -> List (Tag, Type)
getDataTypes ddecl
    = List.map (\(con,ty) -> (con, Types.generalize Set.empty ty)) ddecl.alternatives

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
    = Dict.fromList [ ("Int", KindStar), ("Char", KindStar),
                      ("(,)", kindArgs 2),
                      ("(,,)", kindArgs 3),
                      ("[]", kindArgs 1)
                    ]

-- kind for an n-argument type constructor      
kindArgs : Int -> Kind
kindArgs n = if n > 0 then KindFun KindStar (kindArgs (n-1)) else KindStar
      
-- initial typing environment for primitives
initialTypeEnv : TyEnv
initialTypeEnv
    = let
        intOp = TyFun tyInt (TyFun tyInt tyInt)
        -- NB: no typeclasses so these types are overly polymorphic!
        a = TyGen 0
        b = TyGen 1
        c = TyGen 2
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
      , (":", TyFun a (TyFun (tyList a) (tyList a)))
      , ("[]", tyList a)
      , (",", TyFun a (TyFun b (tyPair a b)))
      , (",,", TyFun a (TyFun b (TyFun c (tyTuple3 a b c))))
      , ("enumFrom", TyFun tyInt (tyList tyInt))
      , ("enumFromTo", TyFun tyInt (TyFun tyInt (tyList tyInt)))
      , ("enumFromThen", TyFun tyInt (TyFun tyInt (tyList tyInt)))
      , ("enumFromThenTo", TyFun tyInt (TyFun tyInt (TyFun tyInt (tyList tyInt))))
      , ("chr", TyFun tyInt tyChar)
      , ("ord", TyFun tyChar tyInt)
      , ("toUpper", TyFun tyChar tyChar)
      , ("toLower", TyFun tyChar tyChar)
      , ("isUpper", TyFun tyChar tyBool)
      , ("isLower", TyFun tyChar tyBool)
      , ("isDigit", TyFun tyChar tyBool)
      , ("isAlpha", TyFun tyChar tyBool)
      , ("isAlphaNum", TyFun tyChar tyBool)
      , ("show", TyFun tyInt tyString) -- only for integers
      , ("force", TyFun a a)
      ]



