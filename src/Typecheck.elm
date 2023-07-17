{-
  Damas Milner type checking and inference 
  Pedro Vasconcelos, 2023
-}

module Typecheck exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Set
import AST exposing (Expr(..), Matching(..), Pattern(..),
                         Program(..), Bind, Name)
import Types exposing (Type(..))
import Tc exposing (Tc, pure, andThen, explain, fail)
import Pretty
import Heap

-- * type environments      
type alias TyEnv
    = Dict AST.Name Type


tcProgram : List Bind -> Program -> Result String ()
tcProgram prelude (Letrec binds expr)
    = let
        env = Dict.union primitiveEnv (makeEnv prelude)
     in
         Tc.eval <|
         (tcRecBind env binds |>
          andThen (\env1 -> tcExpr_ env1 expr |>
                   andThen (\_ -> pure ())))

-- wrapper function that documents the expression being typechecked             
tcExpr_ : TyEnv -> Expr -> Tc Type
tcExpr_ env expr
    = explain ("in expression " ++ Pretty.prettyExpr Heap.empty expr ++ ": ") <|
      tcExpr env expr

             
-- typecheck a single expression      
tcExpr : TyEnv -> Expr -> Tc Type
tcExpr env expr
    = case expr of
          Number _ ->
              pure TyInt
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
                      Tc.fail ("unknown constructor "++tag)
              
          App fun arg ->
              tcExpr env fun |>
              andThen (\tyfun -> tcApplication env tyfun [arg])

          Lam _ match ->
              Tc.freshVar |>
              andThen (\ty -> tcMatching env match ty |>
              andThen (\_ -> pure ty))
                           
          Let binds e1 ->
              tcRecBind env binds |>
              andThen (\env1 -> tcExpr env1 e1)

          Case e1 alts ->
              Tc.fail "not implemented"
                  
              
          IfThenElse e0 e1 e2 ->
              tcExpr env e0 |>
              andThen (\t0 -> Tc.unify t0 TyBool |>
              andThen (\_ -> tcExpr env e1 |>
              andThen (\t1 -> tcExpr env e2 |>
              andThen (\t2 -> Tc.unify t1 t2 |>
              andThen (\_ -> pure t1)))))

          InfixOp op e1 e2 ->
              tcExpr env (App (App (Var op) e1) e2)
          PrefixOp op e1 ->
              tcExpr env (App (Var op) e1)

          Error ->
              Tc.freshVar 

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
                       Pretty.prettyExpr Heap.empty expr ++ ": ")
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
              Tc.unify ty TyInt |>
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
    = Tc.traverse tcRecType binds |>
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
tcRecType : Bind -> Tc Type
tcRecType bind
    = case bind.typeSig of
          Nothing ->
              Tc.freshVar
          Just tysig ->
              pure tysig


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
-- Prelude stuff
------------------------------------------------------------------------------
-- make a type environment from a list of bindings
makeEnv : List Bind -> TyEnv
makeEnv binds
    = List.foldl addTypeSig Dict.empty binds
                 
addTypeSig : Bind -> TyEnv -> TyEnv
addTypeSig bind tyenv
    = case bind.typeSig of
          Just ty ->
              Dict.insert bind.name (Types.generalize Set.empty ty) tyenv
          _ ->
              tyenv


-- type environment for primitives
primitiveEnv : TyEnv
primitiveEnv
    = let
        intOp = TyFun TyInt (TyFun TyInt TyInt)
        cmpOp = TyFun TyInt (TyFun TyInt TyBool)
      in
      Dict.fromList
      [ ("+", intOp), ("*", intOp), ("-", intOp), 
        ("mod", intOp), ("div", intOp), ("negate", TyFun TyInt TyInt)
      , ("==", cmpOp), ("/=", cmpOp), ("<=", cmpOp)
      , (">=", cmpOp), ("<", cmpOp), (">", cmpOp)
      , ("True", TyBool), ("False", TyBool)   
      , (":", TyFun (TyGen 0) (TyFun (TyList (TyGen 0)) (TyList (TyGen 0))))
      , ("[]", TyList (TyGen 0))
      -- TODO: generalize this for more tuple sizes
      , (",", TyFun (TyGen 0) (TyFun (TyGen 1) (TyTuple [TyGen 0, TyGen 1])))
      , ("enumFrom", TyFun TyInt (TyList TyInt))
      , ("enumFromTo", TyFun TyInt (TyFun TyInt (TyList TyInt)))
      , ("enumFromThen", TyFun TyInt (TyFun TyInt (TyList TyInt)))
      , ("enumFromThenTo", TyFun TyInt (TyFun TyInt (TyFun TyInt (TyList TyInt))))
      ]
