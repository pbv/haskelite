{-
  Pretty-printer for Haskelite expressions, types
  and machine configurations
  Pedro Vasconcelos 2021-23
-}
module PrettyOld exposing (..)

import AST exposing (Expr(..), Matching(..), Bind, Pattern(..), Name)
import Types exposing (Type(..))
import Machine.Types exposing (..)
import Machine.Heap exposing (Heap)
import Machine.Heap as Heap
import DList exposing (DList)
import Set exposing (Set)

    
type alias Options
    = { prettyLists : Bool     -- should we prettify strings?
      , prettyEnums : Bool     -- should we prettify Prelude enum functions?
      }

defaultOpts : Options
defaultOpts = { prettyLists = True, prettyEnums = True }

type alias Prec                -- precedence for placing parenthesis
    = Int
              
type alias PrettyCtx
    = { prec : Prec            -- precedence level (0, 1, ..)
      , heap : Heap            -- current heap (for short-circuiting indirections)
      , options : Options
      }
    
-- a type for strings with efficient concatenation
-- i.e. difference lists of strings
type alias StringBuilder
    = DList String

      
-- convert a string builder back into a string      
toString : StringBuilder -> String
toString sb
    = String.concat (DList.toList sb)

-- optionaly add parenthesis around a stringbuilder      
paren : Bool -> StringBuilder -> StringBuilder
paren cond sb
    = if cond then
          bracket "(" ")" sb
      else
          sb

-- bracket a stringbuild with start and end delimiters
bracket : String -> String -> StringBuilder -> StringBuilder
bracket start end sb
    = DList.cons start (DList.append sb (DList.singleton end))


-- toplevel function for pretty printing expressions 
prettyExpr : Options -> Heap -> Expr -> String
prettyExpr opts h e
    = let ctx = { prec = 0, heap = h, options = opts }
      in 
          toString (prettyExpr_ ctx e)

-- worker function                 
prettyExpr_ : PrettyCtx -> Expr -> StringBuilder
prettyExpr_ ctx e =
    case e of 
        Number n ->
            paren (ctx.prec>0 && n<0) <| DList.singleton (String.fromInt n)

        Char c ->
            DList.singleton (prettyChar c)

        Var x ->
            if Heap.isIndirection x then
                case Heap.get x ctx.heap of
                    Just e1 ->
                        prettyExpr_ ctx e1
                    Nothing ->
                        DList.singleton x
                        -- dangling pointer; this should never happen!
            else
                paren (AST.isOperator x) (DList.singleton x)

        Cons "," args ->
            bracket "(" ")" <|
                 (DList.intersperse
                      (DList.singleton ",")                      
                      (List.map (prettyExpr_ {ctx|prec=0}) args))
        Cons ",," args ->
            bracket "(" ")" <|
                 (DList.intersperse
                      (DList.singleton ",")                      
                      (List.map (prettyExpr_ {ctx|prec=0}) args))


        Cons ":" [e1, e2] ->
            if ctx.options.prettyLists then
                case checkSpine e2 of
                    Just l ->
                        case checkChars (e1::l) of
                            Just s ->
                                DList.singleton (String.append "\""
                                                     (String.append s "\""))
                            Nothing ->
                                prettyList ctx (e1::l)
                                
                    Nothing ->
                        paren (ctx.prec>0)
                            (DList.append (prettyExpr_ {ctx|prec=1} e1)
                                 (DList.cons ":" (prettyExpr_ {ctx|prec=1} e2))) 
              else
                paren (ctx.prec>0)
                    (DList.append (prettyExpr_ {ctx|prec=1} e1)
                         (DList.cons ":" (prettyExpr_ {ctx|prec=1} e2)))

        Cons tag [] ->
            DList.singleton tag
                    
        Cons tag args ->
            paren (ctx.prec>0) <|
                (DList.intersperse (DList.singleton " ")
                         ((DList.singleton tag) ::
                          (List.map (prettyExpr_ {ctx|prec=1}) args)))

        BinaryOp op e1 e2 ->
            paren (ctx.prec>0)
                <| if AST.isOperator op then
                       DList.append (prettyExpr_ {ctx|prec=1} e1)
                           (DList.cons op (prettyExpr_ {ctx|prec=1} e2))
                   else
                       DList.cons (op ++ " ")
                           (DList.append
                                (prettyExpr_ {ctx|prec=1} e1)
                                (DList.cons " "
                                     (prettyExpr_ {ctx|prec=1} e2)))

        UnaryOp op e1 ->
            paren (ctx.prec>0) <|
                DList.cons (String.append op " ") (prettyExpr_ {ctx|prec=1} e1)

        App (Var "enumFrom") e1 ->
            if ctx.options.prettyEnums then 
                bracket "[" "..]" <|  prettyExpr_ {ctx|prec=0} e1
            else
                prettyApp ctx (Var "enumFrom") e1

        App (App (Var "enumFromThen") e1) e2 ->
            if ctx.options.prettyEnums then
                bracket "[" "..]" <|
                    DList.append (prettyExpr_ {ctx|prec=0} e1)
                        (DList.cons "," (prettyExpr_ {ctx|prec=0} e2))
            else
               prettyApp ctx (App (Var "enumFromThen") e1) e2 
                
        App (App (Var "enumFromTo") e1) e2 ->
            if ctx.options.prettyEnums then 
                bracket "[" "]" <|
                    DList.append
                        (prettyExpr_ {ctx|prec=0} e1)
                        (DList.cons ".." (prettyExpr_ {ctx|prec=1} e2))
            else
                prettyApp ctx (App (Var "enumFromTo") e1) e2
                            
        App (App (App (Var "enumFromThenTo") e1) e2) e3 ->
            if ctx.options.prettyEnums then
                bracket "[" "]" <|
                    DList.append 
                        (prettyExpr_ {ctx|prec=0} e1)
                        (DList.cons ","
                             (DList.append                      
                                  (prettyExpr_ {ctx|prec=0} e2)
                                  (DList.cons ".."
                                       (prettyExpr_ {ctx|prec=0} e3))))
            else
                prettyApp ctx  (App (App (Var "enumFromThenTo") e1) e2) e3 
           

        App (App (Var op) e1) e2 ->
            if AST.isOperator op then
                paren (ctx.prec>0) <|
                    DList.append (prettyExpr_ {ctx|prec=1} e1)
                        (DList.cons op
                             (prettyExpr_ {ctx|prec=1} e2))
            else
                paren (ctx.prec>0) <|
                    DList.append (prettyExpr_ {ctx|prec=0} (App (Var op) e1))
                        (DList.append (DList.singleton " ") (prettyExpr_ {ctx|prec=1} e2))
                        
        App e0 e1 ->
            prettyApp ctx e0 e1

        Lam _ optid match ->
            case collectArgs match [] of
                (_, []) ->
                    prettyLam ctx optid match
                (match1, args1) ->
                    let
                        expr1 = List.foldl (\x y->App y x) (AST.lambda optid match1)  args1
                    in 
                        prettyExpr_ ctx expr1

        Let binds e1 ->
             DList.cons "let "
                (DList.append (prettyBinds ctx binds)
                     (DList.cons " in " (prettyExpr_ {ctx|prec=0} e1)))
        Case expr alts ->
            paren (ctx.prec>0) <|
            DList.cons "case "
                (DList.append
                     (prettyExpr_ {ctx|prec=1} expr)
                     (DList.cons " of " (prettyAlts {ctx|prec=1} alts)))
                    
        IfThenElse e1 e2 e3 ->
            paren (ctx.prec>0)
                <|  DList.cons "if "
                    (DList.append (prettyExpr_ {ctx|prec=0} e1)
                         (DList.cons " then "
                              (DList.append
                                   (prettyExpr_ {ctx|prec=0} e2)
                                   (DList.cons " else "
                                        (prettyExpr_ {ctx|prec=0} e3)))))

        Error e1 ->
            DList.cons "error " (prettyExpr_ ctx e1)

prettyList : PrettyCtx -> List Expr -> StringBuilder
prettyList ctx exprs
    = DList.concat <|
      (DList.singleton "[") ::
          ((List.intersperse (DList.singleton ",") <|
               List.map (prettyExpr_ {ctx|prec=0}) exprs)
               ++
               [DList.singleton "]"])
                

-- pretty print a generic application
prettyApp : PrettyCtx -> Expr -> Expr -> StringBuilder
prettyApp ctx e0 e1
    = paren (ctx.prec>0) <|
      DList.append (prettyExpr_ {ctx|prec=0} e0)
          (DList.append (DList.singleton " ") (prettyExpr_ {ctx|prec=1} e1))



             
-- auxiliary function to collect arguments to a matching
collectArgs : Matching -> List Expr -> (Matching, List Expr)
collectArgs m args
    = case m of
          Arg e1 m1 ->
              collectArgs m1 (e1::args)
          _ ->
              (m, args)

-- pretty print case alternatives
prettyAlts : PrettyCtx -> List (Pattern,Expr) -> StringBuilder
prettyAlts ctx alts
    = case alts of
          [] -> DList.empty
          [first] ->
              prettyAlt ctx first
          (first::rest) ->
              DList.append (prettyAlt ctx first)
                  (DList.cons "; " (prettyAlts ctx rest))
                  

prettyAlt : PrettyCtx -> (Pattern,Expr) -> StringBuilder
prettyAlt ctx (patt,expr)
    = DList.append (prettyPattern patt)
             (DList.cons " -> " (prettyExpr_ {ctx|prec=0} expr))
                


-- pretty print a lambda
prettyLam : PrettyCtx -> Maybe Name -> Matching -> StringBuilder
prettyLam ctx optid m
    = case optid of
          -- just use the binding name if there is one
          Just id ->
              DList.singleton id
          Nothing ->
              -- otherwise check if it has any arguments
              case m of
                  Match p m1 ->
                      paren True <| DList.cons "\\" (prettyMatch ctx m)
                  Return e _ ->
                      prettyExpr_ ctx e
                  _ ->
                      DList.singleton "<unimplemented1>"
          
-- pretty print a matching            
prettyMatch : PrettyCtx -> Matching -> StringBuilder
prettyMatch ctx m =
    case m of
        (Match p m1) ->
            DList.append (prettyPattern p)
                (DList.cons " " (prettyMatch ctx m1))
        (Return e _) ->
            DList.cons "-> " (prettyExpr_ {ctx|prec=0} e)

        _ ->
            DList.singleton "<unimplemented2>"


-- pretty print a list of bindings
prettyBinds : PrettyCtx -> List Bind -> StringBuilder
prettyBinds ctx binds
    = case binds of
          [] ->
              DList.empty
          [first] ->
              prettyBind ctx first
          (first::rest) ->
              DList.append (prettyBind ctx first)
                  (DList.cons "; " (prettyBinds ctx rest))

prettyBind : PrettyCtx -> Bind -> StringBuilder
prettyBind ctx bind
    = DList.cons (bind.name ++ " = ") (prettyExpr_ {ctx|prec=0} bind.expr)
                

-- format an infix operator, sometimes with spaces either side
formatOperator : Name -> String
formatOperator op
    = if op=="&&" || op == "||"  then " " ++ op ++ " " else op



-- pretty print a pattern                   
prettyPattern : Pattern -> StringBuilder
prettyPattern p =
    case p of
        DefaultP ->
            DList.singleton "_"
        VarP x ->
            DList.singleton x
        BangP x ->
            DList.singleton ("!"++x)
        NumberP n ->
            DList.singleton (String.fromInt n)
        CharP c ->
            DList.singleton (prettyChar c)
                
        ConsP "," ps ->
            bracket "(" ")" <|
                DList.intersperse
                    (DList.singleton ",")
                    (List.map prettyPattern ps)
        ConsP ",," ps ->
            bracket "(" ")" <|
                DList.intersperse
                    (DList.singleton ",")
                    (List.map prettyPattern ps)

        ConsP ":" [p1,p2] ->
            bracket "(" ")" <|
                DList.append (prettyPattern p1)
                    (DList.cons ":" (prettyPattern p2))
        ConsP tag [] ->
            DList.singleton tag
        ConsP tag ps ->
            bracket "(" ")" <|
                DList.intersperse (DList.singleton " ")
                    ((DList.singleton tag) :: (List.map prettyPattern ps))
                
           

prettyType : Type -> String
prettyType ty = toString (prettyType_ 0 ty)

prettyType_ : Prec -> Type -> StringBuilder
prettyType_ prec ty
    = case ty of
          TyConst c [] ->
              DList.singleton c
          TyConst "[]" [ty1] ->
              bracket "[" "]" (prettyType_ 0 ty1)
          TyConst "(,)" ts ->
              bracket "(" ")" <|
                  DList.intersperse
                      (DList.singleton ",")
                      (List.map (prettyType_ 0) ts) 
          TyConst "(,,)" ts ->
              bracket "(" ")" <|
                  DList.intersperse
                      (DList.singleton ",")
                      (List.map (prettyType_ 0) ts) 
              
          TyConst c ts ->
              paren (prec>0) 
                  (DList.intersperse (DList.singleton " ")
                       (DList.singleton c :: List.map (prettyType_ 1) ts))
                      
          TyVar name ->
              DList.singleton name

          TyGen idx ->
              DList.singleton (showGenVar idx)

          TyFun t1 t2 ->
              paren (prec>0) <|
                  DList.append (prettyType_ 1 t1)
                      (DList.cons "->" (prettyType_ 0 t2))

showGenVar : Int -> String
showGenVar n
    = String.fromChar <| Char.fromCode <| Char.toCode 'a' + n


-- check if an cons expression has an evaluated spine
-- in that case return the list of expressions
checkSpine : Expr -> Maybe (List Expr)
checkSpine e
    = case e of
          Cons "[]" [] ->
              Just []
          Cons ":" [hd,tl] ->
                      checkSpine tl
                      |> Maybe.andThen (\l -> Just (hd::l))
          _ ->
              Nothing

-- check if a list of expressions consists only of characters
checkChars : List Expr -> Maybe String
checkChars es =
    if List.all isChar es then
        Just (String.concat (List.map (getChar >> escapeChar '"') es))
    else
        Nothing


isChar : Expr -> Bool
isChar e =
    case e of
        Char _ -> True
        _ -> False
            
getChar : Expr -> Char
getChar e =
    case e of
        Char c -> c
        _ -> '?'   -- this never happens!

      

----------------------------------------------------------------------------------
-- showing configurations 
----------------------------------------------------------------------------------
                  
prettyConf : Options -> Conf ->  Maybe String
prettyConf opts (heap, control, stack)
   = case (control, stack) of
         (E expr, _) ->
             Just <| prettyCont opts heap stack expr
         _ ->
             Nothing
             

                 
-- convert a continuation stack into a string
prettyCont : Options -> Heap -> Stack -> Expr -> String
prettyCont opts heap stack acc
    = case stack of
          [] ->
              prettyExpr opts heap acc
          (Update _::rest) ->
              prettyCont opts heap rest acc
          (PushArg arg::rest) ->
              prettyCont opts heap rest (App acc arg)
          (ContBinary op e2::rest) ->
              prettyCont opts heap rest (BinaryOp op acc e2)
          (RetBinary op e1::rest) ->
              prettyCont opts heap rest (BinaryOp op e1 acc)
          (RetUnary op::rest) ->
              prettyCont opts heap rest (UnaryOp op acc)
          MatchEnd::rest ->
              prettyCont opts heap rest acc
          DeepEval::rest ->
              prettyCont opts heap rest acc
          Continue expr ctx::rest ->
              prettyCont opts heap rest (ctx.set acc expr)
          (_::rest) ->
              "... " ++ prettyExpr opts heap acc



-- pretty print a character
prettyChar : Char -> String
prettyChar c
    = "\'" ++ escapeChar '\'' c ++ "\'"

escapeChar : Char -> Char -> String
escapeChar delimiter c
    = if c == delimiter then
          "\\" ++ String.fromChar delimiter
      else case c of
          '\n' -> "\\n"
          '\t' -> "\\t"
          '\\' -> "\\\\"
          _ -> let n = Char.toCode c
               in if n>=32 && n<=127 then
                      String.fromChar c
                  else
                      "\\" ++ String.fromInt n
