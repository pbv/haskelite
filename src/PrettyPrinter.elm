{-
  Pretty-printer for Haskelite expressions, types and machine configurations
  Pedro Vasconcelos 2021-23
-}
module PrettyPrinter exposing (..)

import AST exposing (Expr(..), Matching(..), Bind, Pattern(..), Name)
import Types exposing (Type(..))
import Machine.Types exposing (..)
import Machine.Heap exposing (Heap)
import Machine.Heap as Heap
import Pretty exposing (Doc, string, char, space, join, parens, brackets, a,
                            words, lines)
import Set exposing (Set)

   
type alias Options
    = { prettyLists : Bool     -- should we prettify lists?
      , prettyEnums : Bool     -- should we prettify Prelude enum functions?
      }

defaultOpts : Options
defaultOpts
    = { prettyLists = True, prettyEnums = True }

type alias Prec                -- precedence for placing parenthesis
    = Int
              
type alias PrettyCtx
    = { prec : Prec            -- precedence level (0, 1, ..)
      , heap : Heap            -- current heap (for short-circuiting indirections)
      , options : Options
      }


-- conditionally add parenthesis to a document
parensIf : Bool -> Doc t -> Doc t
parensIf c doc
    = if c then Pretty.parens doc else doc


-- a very long line length for pretty-printing without breaks
defaultLength : Int
defaultLength
    = 9999

--
-- top-level functions to render to a string
--
prettyType : Type -> String
prettyType ty
    = Pretty.pretty defaultLength (ppType 0 ty)
      
prettyExpr : Options -> Heap -> Expr -> String
prettyExpr opts h e
    = let ctx = { prec=0, heap=h, options=opts }
      in 
          Pretty.pretty defaultLength (ppExpr ctx e)

prettyPattern : Pattern -> String
prettyPattern p
    = Pretty.pretty defaultLength (ppPattern p)

--
-- configurations 
--
prettyConf : Options -> Conf -> Maybe String
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


                
              
              
-- functions to pretty-print to a document              

ppExpr : PrettyCtx -> Expr -> Doc t
ppExpr ctx e =
    case e of 
        Number n ->
            parensIf (ctx.prec>0 && n<0) <| string <| String.fromInt n

        Char c ->
            string (prettyChar c)

        Var x ->
            if Heap.isIndirection x then
                case Heap.get x ctx.heap of
                    Just e1 ->
                        ppExpr ctx e1
                    Nothing ->
                        string (x ++ "?")
                        -- dangling pointer; this should never happen!
            else
                parensIf (AST.isOperator x) (string x)

        Cons "," args ->
               parens <| join (string ",") <|
                    List.map (ppExpr {ctx|prec=0}) args
        Cons ",," args ->
                  parens <| join (string ",") <|
                      List.map (ppExpr {ctx|prec=0}) args


        Cons ":" [e1, e2] ->
            if ctx.options.prettyLists then
                case checkSpine e2 of
                    Just l ->
                        case checkChars (e1::l) of
                            Just s ->
                                string ("\"" ++ s ++ "\"")
                            Nothing ->
                                ppList ctx (e1::l)
                                
                    Nothing ->
                        parensIf (ctx.prec>0) <|
                            (ppExpr {ctx|prec=1} e1
                                |> a (char ':')
                                |> a (ppExpr {ctx|prec=1} e2))
              else
                parensIf (ctx.prec>0) <|
                    (ppExpr {ctx|prec=1} e1
                         |> a (char ':')
                         |> a (ppExpr {ctx|prec=1} e2))

        Cons tag [] ->
            string tag
                    
        Cons tag args ->
            parensIf (ctx.prec>0) <|
                words <| (string tag) ::
                          (List.map (ppExpr {ctx|prec=1}) args)

        BinaryOp op e1 e2 ->
            parensIf (ctx.prec>0) <| -- if AST.isOperator op then
                (ppExpr {ctx|prec=1} e1
                           |> a (string op)
                           |> a (ppExpr {ctx|prec=1} e2))
                       {-
                   else
                       DList.cons (op ++ " ")
                           (DList.append
                                (ppExpr {ctx|prec=1} e1)
                                (DList.cons " "
                                     (ppExpr {ctx|prec=1} e2)))
                        -}

        UnaryOp op e1 ->
            parensIf (ctx.prec>0) <|
                (string op |> a space |> a (ppExpr {ctx|prec=1} e1))

        App (Var "enumFrom") e1 ->
            if ctx.options.prettyEnums then 
                brackets <| (ppExpr {ctx|prec=0} e1 |> a (string ".."))
            else
                ppApp ctx (Var "enumFrom") e1

        App (App (Var "enumFromThen") e1) e2 ->
            if ctx.options.prettyEnums then
                brackets  <|
                    (ppExpr {ctx|prec=0} e1
                           |> a  (char ',')
                           |> a (ppExpr {ctx|prec=0} e2)
                           |> a (string ".."))
            else
               ppApp ctx (App (Var "enumFromThen") e1) e2 
                
        App (App (Var "enumFromTo") e1) e2 ->
            if ctx.options.prettyEnums then 
                brackets <|
                    (ppExpr {ctx|prec=0} e1
                         |> a (string "..")
                         |> a (ppExpr {ctx|prec=1} e2))
            else
                ppApp ctx (App (Var "enumFromTo") e1) e2
                            
        App (App (App (Var "enumFromThenTo") e1) e2) e3 ->
            if ctx.options.prettyEnums then
                brackets <|
                    (ppExpr {ctx|prec=0} e1
                         |> a (char ',')
                         |> a (ppExpr {ctx|prec=0} e2)
                         |> a (string "..")
                         |> a (ppExpr {ctx|prec=0} e3))
            else
                ppApp ctx  (App (App (Var "enumFromThenTo") e1) e2) e3 
           
        App (App (Var op) e1) e2 ->
            if AST.isOperator op then
                parensIf (ctx.prec>0) 
                    (ppExpr {ctx|prec=1} e1
                        |> a (string op)
                        |> a (ppExpr {ctx|prec=1} e2))
            else
                parensIf (ctx.prec>0)
                    (ppExpr {ctx|prec=0} (App (Var op) e1)
                        |> a space
                        |> a (ppExpr {ctx|prec=1} e2))

        App e0 e1 ->
            ppApp ctx e0 e1
                
        Lam _ optid match ->
            case collectArgs match [] of
                (_, []) ->
                    prettyLam ctx optid match
                (match1, args1) ->
                    let
                        expr1 = List.foldl (\x y->App y x)
                                           (AST.lambda optid match1)  args1
                    in 
                        ppExpr ctx expr1

        Let binds e1 ->
             string "let"
                 |> a space
                 |> a (prettyBinds ctx binds)
                 |> a space |> a (string "in") |> a space
                 |> a (ppExpr {ctx|prec=0} e1)

     
        Case expr alts ->
            parensIf (ctx.prec>0) 
                (string "case"
                    |> a space
                    |> a (ppExpr {ctx|prec=1} expr)
                    |> a space
                    |> a (string "of")
                    |> a space
                    |> a (prettyAlts {ctx|prec=1} alts))
                    
        IfThenElse e1 e2 e3 ->
            parensIf (ctx.prec>0)
                (string "if"
                     |> a (ppExpr {ctx|prec=0} e1)
                     |> a space
                     |> a (string "then")
                     |> a space
                     |> a (ppExpr {ctx|prec=0} e2)
                     |> a space
                     |> a (string "else")
                     |> a space
                     |> a (ppExpr {ctx|prec=0} e3))

        Error e1 ->
            string "error" |> a space |> a (ppExpr ctx e1)



ppList : PrettyCtx -> List Expr -> Doc t
ppList ctx exprs
    = Pretty.brackets <|
          Pretty.join (string ",") <|
               List.map (ppExpr {ctx|prec=0}) exprs

                
-- pretty print a generic application
ppApp : PrettyCtx -> Expr -> Expr -> Doc t
ppApp ctx e0 e1
    = parensIf (ctx.prec>0) <|
         (ppExpr {ctx|prec=0} e0
             |> a space
             |> a (ppExpr {ctx|prec=1} e1))


           
-- auxiliary function to collect arguments to a matching
collectArgs : Matching -> List Expr -> (Matching, List Expr)
collectArgs m args
    = case m of
          Arg e1 m1 ->
              collectArgs m1 (e1::args)
          _ ->
              (m, args)


-- pretty print case alternatives
prettyAlts : PrettyCtx -> List (Pattern,Expr) -> Doc t
prettyAlts ctx alts
    = lines (List.map (prettyAlt ctx) alts)

prettyAlt : PrettyCtx -> (Pattern,Expr) -> Doc t
prettyAlt ctx (patt,expr)
    = ppPattern patt
          |> a (string "->")
          |> a (ppExpr {ctx|prec=0} expr)


-- pretty print a lambda
prettyLam : PrettyCtx -> Maybe Name -> Matching -> Doc t
prettyLam ctx optid m
    = case optid of
          -- just use the binding name if there is one
          Just id ->
              string id
          Nothing ->
              -- otherwise check if it has any arguments
              case m of
                  Match p m1 ->
                      parens <| (char '\\' |> a (prettyMatch ctx m))
                  Return e _ ->
                      ppExpr ctx e
                  _ ->
                      string "<unimplemented1>"

          
-- pretty print a matching            
prettyMatch : PrettyCtx -> Matching -> Doc t
prettyMatch ctx m =
    case m of
        (Match p m1) ->
            ppPattern p
                 |> a space
                 |> a (prettyMatch ctx m1)
        (Return e _) ->
            string "->"
                  |> a (ppExpr {ctx|prec=0} e)

        _ ->
            string "<unimplemented2>"


-- pretty print a list of bindings
prettyBinds : PrettyCtx -> List Bind -> Doc t
prettyBinds ctx binds
    = lines (List.map (prettyBind ctx) binds)

prettyBind : PrettyCtx -> Bind -> Doc t
prettyBind ctx bind
    = string (bind.name)
          |> a space
          |> a (char '=') 
          |> a space 
          |> a (ppExpr {ctx|prec=0} bind.expr)
                

-- format an infix operator, sometimes with spaces either side
formatOperator : Name -> String
formatOperator op
    = if op=="&&" || op == "||"  then " " ++ op ++ " " else op
                

-- pretty-print a type
ppType : Prec -> Type -> Doc t
ppType prec ty
    = case ty of
          TyConst c [] ->
              string c
          TyConst "[]" [ty1] ->
              brackets (ppType 0 ty1)
          TyConst "(,)" ts ->
              parens  <|
                  join (char ',') (List.map (ppType 0) ts) 
          TyConst "(,,)" ts ->
              parens <|
                  join (char ',') (List.map (ppType 0) ts) 
              
          TyConst c ts ->
              parensIf (prec>0) 
                  (words 
                       (string c :: List.map (ppType 1) ts))
                      
          TyVar name ->
              string name

          TyGen idx ->
              string (showGenVar idx)

          TyFun t1 t2 ->
              parensIf (prec>0) <|
                  (ppType 1 t1
                      |> a (string "->")
                      |> a (ppType 0 t2))

showGenVar : Int -> String
showGenVar n
    = String.fromChar <| Char.fromCode <| Char.toCode 'a' + n



-- pretty print a pattern                   
ppPattern : Pattern -> Doc t
ppPattern p =
    case p of
        DefaultP ->
            char '_'
        VarP x ->
            string x
        BangP x ->
            string ("!"++x)
        NumberP n ->
            string (String.fromInt n)
        CharP c ->
            string (prettyChar c)
                
        ConsP "," ps ->
            parens <|
                join (string ",") (List.map ppPattern ps)
        ConsP ",," ps ->
            parens <|
                join (string ",") (List.map ppPattern ps)

        ConsP ":" [p1,p2] ->
            parens <|
                (ppPattern p1
                       |> a (char ':')
                       |> a (ppPattern p2))
        ConsP tag [] ->
            string tag
        ConsP tag ps ->
            parens <|
                words <| (string tag :: List.map ppPattern ps)


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
