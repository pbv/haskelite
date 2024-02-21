{-
  Pretty-printer for Haskelite expressions, types and machine configurations
  Pedro Vasconcelos 2021--24
-}
module PrettyPrinter exposing (prettyExpr, prettyPattern, prettyType, prettyConfStep)

import AST exposing (Expr(..), Matching(..), Info, Bind, Pattern(..), Name)
import Types exposing (Type(..))
import Machine.Types exposing (..)
import Machine.Heap exposing (Heap)
import Machine.Heap as Heap
import Pretty exposing (Doc, string, taggedString, char, space, join,
                        parens, brackets, a, words, 
                        nest, indent, align, hang, group)
import Pretty.Renderer exposing (Renderer)

import Html exposing (Html, Attribute)
import Html.Attributes exposing (class)
   
type alias Options
    = { prettyLists : Bool     -- should we prettify lists?
      , prettyEnums : Bool     -- should we prettify prelude enum* functions?
      , layout : Bool          -- use layout?
      , justifications : Bool  -- show justifications inline?
      }

type alias Prec                -- precedence for placing parenthesis
    = Int
              
type alias PrettyCtx
    = { prec : Prec            -- precedence level (0, 1, ..)
      , heap : Heap            -- current heap (for short-circuiting indirections)
      , prettyLists : Bool
      , prettyEnums : Bool
        -- methods for handling layout
      , separators : String -> List (Doc Tag) -> Doc Tag
      , line : Doc Tag
      , lines : List (Doc Tag) -> Doc Tag
      }


makeCtx : Options -> Heap -> PrettyCtx
makeCtx opts heap
    = { prec = 0
      , heap = heap
      , prettyLists = opts.prettyLists
      , prettyEnums = opts.prettyEnums
      , separators = if opts.layout then Pretty.separators
                     else \sep -> Pretty.join (string sep)
      , line = if opts.layout then Pretty.line else Pretty.space
      , lines = if opts.layout then Pretty.lines else Pretty.words
      }

           
-- tags for expression highlighting
type Tag = Keyword
         | Literal
         | String
         | Constructor
         | Variable
         | Linenumber
         | Exception

           
-- conditionally add parenthesis to a document
parensIf : Bool -> Doc t -> Doc t
parensIf c doc
    = if c then Pretty.parens doc else doc
     
-- default line length for pretty-printing 
defaultLength : Int
defaultLength
    = 70

--
-- top-level functions to render to a string
--
prettyType : Type -> String
prettyType ty
    = Pretty.pretty defaultLength (ppType 0 ty)
      
prettyExpr : Options -> Heap -> Expr -> String
prettyExpr opts heap e
    = let ctx = makeCtx opts heap
      in Pretty.pretty defaultLength (ppExpr ctx e)

prettyPattern : Pattern -> String
prettyPattern p
    = Pretty.pretty defaultLength (ppPattern p)

--
-- render a configuration to HTML
--
prettyConfStep : Options -> Conf -> Int -> Maybe (Html msg)
prettyConfStep opts conf step
    = case ppConfStep opts conf step of
          Just doc ->
              Just <| Pretty.Renderer.pretty defaultLength htmlRenderer doc
          Nothing ->
              Nothing

-- pretty print a configuration step
ppConfStep : Options -> Conf -> Int -> Maybe (Doc Tag)
ppConfStep opts conf step
    = case ppConf opts conf of
          Just doc ->
              if step>0 then 
                  Just (taggedString "=" Linenumber
                       |> a space
                       |> a (align doc))
              else
                  Just (space |> a space |> a (align doc))
          Nothing ->
              Nothing
                  

-- pretty print a configuration
ppConf : Options -> Conf -> Maybe (Doc Tag)
ppConf opts (heap, control, stack)
    = case (getExpr control) of
         Just expr ->
             let ppCtx = makeCtx opts heap 
             in 
             case unwindStack stack expr of
                 ([], expr1) ->
                     Just (ppExpr ppCtx expr1)
                 (stk,expr1) ->
                     let ellipsis = String.repeat (List.length stk) "."
                     in Just (taggedString ellipsis Linenumber
                             |> a space     
                             |> a (align (ppExpr ppCtx expr1)))
         _ ->
             Nothing

getExpr : Control -> Maybe Expr
getExpr ctrl
    = case ctrl of
          E expr ->
              Just expr
          M (Return expr _) _ ->
              Just expr
          _ ->
              Nothing
                 
-- unwind a stack into a context expression;
-- may stop earlier if we reach a guard/case alternative
unwindStack : Stack -> Expr -> (Stack, Expr)
unwindStack stack acc
    = case stack of
          (Update _::rest) ->
              unwindStack rest acc
          (PushArg arg::rest) ->
              unwindStack rest (App acc arg)
          (ContBinary1 op e2::rest) ->
              unwindStack rest (BinaryOp op acc e2)
          (ContBinary2 op e1::rest) ->
              unwindStack rest (BinaryOp op e1 acc) 
          (RetBinary op _ _ ::rest) ->
              unwindStack rest acc
          (ContUnary op::rest) ->
              unwindStack rest (UnaryOp op acc)
          (RetUnary op _ ::rest) ->
              unwindStack rest acc              
          (MatchEnd::rest) ->
              unwindStack rest acc
          (DeepEval::rest) ->
              unwindStack rest acc
          (Continue expr ctx::rest) ->
              unwindStack rest (ctx.set acc expr)
          _ ->
              (stack, acc)
               
-- pretty-print an expression 
ppExpr : PrettyCtx -> Expr -> Doc Tag
ppExpr ctx e =
    case e of 
        Number n ->
            parensIf (ctx.prec>0 && n<0) <|
                taggedString (String.fromInt n) Literal

        Char c ->
            taggedString (prettyChar c) String

        Var x ->
            if Heap.isIndirection x then
                case Heap.get x ctx.heap of
                    Just e1 ->
                        -- delete the indirection from the heap
                        -- to avoid infinite recursion
                        ppExpr {ctx|heap=Heap.delete x ctx.heap} e1
                    Nothing ->
                        -- maybe print an ellipsis instead of the name?
                        taggedString x Exception
            else
                if AST.isOperator x then
                    parens (string x)
                else
                    taggedString x Variable

        Cons _ "," args ->
            group (parens <| ctx.separators ", " <|
                       List.map (ppExpr {ctx|prec=0}) args)
        Cons _ ",," args ->
            group (parens <| ctx.separators ", " <|
                       List.map (ppExpr {ctx|prec=0}) args)

        Cons _ ",,," args ->
            group (parens <| ctx.separators ", " <|
                       List.map (ppExpr {ctx|prec=0}) args)
                
        Cons _ ":" [e1, e2] ->
            if ctx.prettyLists then
                case checkSpine e2 of
                    Just l ->
                        case checkChars (e1::l) of
                            Just s ->
                                taggedString ("\"" ++ s ++ "\"") String
                            Nothing ->
                                ppList ctx (e1::l)
                                
                    Nothing ->
                        parensIf (ctx.prec>0) 
                            (ppExpr {ctx|prec=1} e1
                                |> a (taggedString ":" Constructor)
                                |> a (ppExpr {ctx|prec=1} e2))
              else
                parensIf (ctx.prec>0) 
                    (ppExpr {ctx|prec=1} e1
                         |> a (taggedString ":" Constructor)
                         |> a (ppExpr {ctx|prec=1} e2))

        Cons _ tag args ->
            ppCons ctx tag args

        BinaryOp op e1 e2 ->
                parensIf (ctx.prec>0) 
                    (group (ppExpr {ctx|prec=1} e1
                              |> a ctx.line
                              |> a (string (formatOperator op))
                              |> a ctx.line
                              |> a (ppExpr {ctx|prec=1} e2)))

        UnaryOp op e1 ->
            parensIf (ctx.prec>0) 
                (string op |> a space |> a (ppExpr {ctx|prec=1} e1))

        App e0 e1 ->
            ppApp ctx e0 e1
                
        Lam _ optid match ->
            case collectMatchingArgs match [] of
                (_, []) ->
                    ppLambda ctx optid match
                (match1, args1) ->
                    ppApp1 ctx (AST.lambda optid match1) args1

        Let binds e1 ->
             parensIf (ctx.prec>0)
                 (group 
                      (taggedString "let" Keyword
                      |> a ctx.line
                      |> a (ppBinds ctx binds)
                      |> a ctx.line
                      |> a (taggedString "in" Keyword)
                      |> a ctx.line
                      |> a (ppExpr {ctx|prec=0} e1)))
                      
     
        Case expr alts ->
            parensIf (ctx.prec>0) 
                (group (hang 4 (taggedString "case" Keyword
                    |> a space
                    |> a (ppExpr {ctx|prec=1} expr)
                    |> a space 
                    |> a (taggedString "of" Keyword)
                    |> a ctx.line
                    |> a (ppAlts {ctx|prec=0} alts))))
                    
        IfThenElse e1 e2 e3 ->
            parensIf (ctx.prec>0)
                (group (taggedString "if" Keyword
                        |> a space
                        |> a (ppExpr {ctx|prec=0} e1)
                        |> a space
                        |> a (taggedString "then" Keyword)
                        |> a (nest 4 (ctx.line |> a (ppExpr {ctx|prec=0} e2)))
                        |> a ctx.line
                        |> a (taggedString "else" Keyword)
                        |> a (nest 4 (ctx.line |> a (ppExpr {ctx|prec=0} e3)))))

        AST.Exception msg ->
            taggedString ("exception: " ++ msg) Exception


ppCons : PrettyCtx -> String -> List Expr -> Doc Tag
ppCons ctx cons args
    = case args of
          [] ->
              taggedString cons Constructor
          [arg] ->
              parensIf (ctx.prec>0)
                  (taggedString cons Constructor
                      |> a space 
                      |> a (ppExpr {ctx|prec=1} arg))
          _ ->
              parensIf (ctx.prec>0)
                  (group (hang 4 ((taggedString cons Constructor)
                        |> a ctx.line
                        |> a (ctx.lines (List.map (ppExpr {ctx|prec=1}) args)))))

                
          

      
ppList : PrettyCtx -> List Expr -> Doc Tag
ppList ctx exprs
    = brackets <|
      join (string ", ") (List.map (ppExpr {ctx|prec=0}) exprs)

                
-- pretty print a generic application
ppApp : PrettyCtx -> Expr -> Expr -> Doc Tag
ppApp ctx e0 e1
    = let (fun, args) = collectArgs ctx.heap e0 [e1]
      in ppApp1 ctx fun args

ppApp1 : PrettyCtx -> Expr -> List Expr -> Doc Tag
ppApp1 ctx fun args
    = case (nameOf fun,args) of
          -- special cases for enums
          (Just "enumFrom", [e1]) ->
              if ctx.prettyEnums then
                  brackets (ppExpr {ctx|prec=0} e1 |> a (string " .."))
              else
                  ppApp2 ctx fun args
          (Just "enumFromTo", [e1,e2]) ->
              if ctx.prettyEnums then
                  brackets 
                    (ppExpr {ctx|prec=0} e1
                         |> a (string " .. ")
                         |> a (ppExpr {ctx|prec=0} e2))                  
              else
                  ppApp2 ctx fun args
          (Just "enumFromThen", [e1,e2]) ->
              if ctx.prettyEnums then
                brackets  
                    (ppExpr {ctx|prec=0} e1
                           |> a  (char ',')
                           |> a (ppExpr {ctx|prec=0} e2)
                           |> a (string " .."))                  
              else
                  ppApp2 ctx fun args
          (Just "enumFromThenTo", [e1,e2,e3]) ->
              if ctx.prettyEnums then
                  brackets 
                    (ppExpr {ctx|prec=0} e1
                         |> a (char ',')
                         |> a (ppExpr {ctx|prec=0} e2)
                         |> a (string " .. ")
                         |> a (ppExpr {ctx|prec=0} e3))
              else
                  ppApp2 ctx fun args

          -- special cases for binary operators
          (Just op, [arg1, arg2]) ->
              if AST.isOperator op then
                  ppExpr ctx (BinaryOp op arg1 arg2)
              else
                  ppApp2 ctx fun args
          _ ->
              ppApp2 ctx fun args

-- fetch the name for a function (if any)                  
nameOf : Expr -> Maybe Name
nameOf expr 
    = case expr of
          (Var name) ->
              Just name
          (Lam _ (Just name) _) ->
              Just name
          _ ->
              Nothing
                  
ppApp2 : PrettyCtx -> Expr -> List Expr -> Doc Tag                  
ppApp2 ctx fun args
    = parensIf (ctx.prec>0) <|
          group (hang 4 ((ppExpr {ctx|prec=0} fun)
                    |> a space
                    |> a (ctx.lines (List.map (ppExpr {ctx|prec=1}) args))))
                  
                  
              
-- auxiliary function to collect arguments to an application
collectArgs : Heap -> Expr -> List Expr -> (Expr, List Expr)
collectArgs heap e acc
    = case e of
          Var x ->
              case Heap.get x heap of
                  Nothing ->
                      (e, acc)
                  Just ex ->
                      collectArgs (Heap.delete x heap) ex acc
                          
          App e1 arg ->
              collectArgs heap e1 (arg::acc)
          _ ->
              (e, acc)
                  
-- auxiliary function to collect arguments to a matching
collectMatchingArgs : Matching -> List Expr -> (Matching, List Expr)
collectMatchingArgs m acc
    = case m of
          Arg e1 m1 ->
              collectMatchingArgs m1 (e1::acc)
          _ ->
              (m, acc)


-- pretty print case alternatives
ppAlts : PrettyCtx -> List (Pattern,Expr,Info) -> Doc Tag
ppAlts ctx alts
    = join (char ';' |> a ctx.line) (List.map (ppAlt ctx) alts)

ppAlt : PrettyCtx -> (Pattern,Expr,Info) -> Doc Tag
ppAlt ctx (patt, expr, info)
    = ppPattern patt
          |> a (string " -> ")
          |> a (ppExpr {ctx|prec=0} expr)


-- pretty print a lambda
ppLambda : PrettyCtx -> Maybe Name -> Matching -> Doc Tag
ppLambda ctx optid m
    = case optid of
          -- just use the binding name if there is one
          Just name ->
              if AST.isOperator name then
                  parens (string name)
              else
                  taggedString name Variable
          Nothing ->
              -- otherwise check if it has any arguments
              case m of
                  Match p m1 ->
                      parens (char '\\' |> a (ppMatching ctx m))
                  Return e _ ->
                      ppExpr ctx e
                  _ ->
                      taggedString "<unimplemented>" Exception

          
-- pretty print a matching            
ppMatching : PrettyCtx -> Matching -> Doc Tag
ppMatching ctx m =
    case m of
        (Match p m1) ->
            ppPattern p
                 |> a space
                 |> a (ppMatching ctx m1)
        (Return e _) ->
            string "->"
                  |> a space
                  |> a (ppExpr {ctx|prec=0} e)

        _ ->
            string "<unimplemented>"


-- pretty print a list of bindings
ppBinds : PrettyCtx -> List Bind -> Doc Tag
ppBinds ctx binds
    = join (char ';' |> a ctx.line) (List.map (ppBind ctx) binds)

ppBind : PrettyCtx -> Bind -> Doc Tag
ppBind ctx bind
    = string (bind.name)
          |> a space
          |> a (char '=') 
          |> a space 
          |> a (ppExpr {ctx|prec=0} (removeName bind.name bind.expr))


removeName : Name -> Expr -> Expr
removeName name expr
    = case expr of
          Lam arity (Just name1) m ->
              if name == name1 then 
                  Lam arity Nothing m
              else
                  expr
          _ ->
              expr

-- format an infix operator, sometimes with spaces either side
formatOperator : Name -> String
formatOperator op
    = if AST.isOperator op then op else "`" ++op++"`"
                

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
          TyConst "(,,,)" ts ->
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
ppPattern : Pattern -> Doc Tag
ppPattern p =
    case p of
        DefaultP ->
            char '_'
        VarP x ->
            taggedString x Variable
        BangP x ->
            char '!' |> a (taggedString x Variable)
        NumberP n ->
            taggedString (String.fromInt n) Literal
        CharP c ->
            taggedString (prettyChar c) String
                
        ConsP "," ps ->
            parens <|
                join (string ",") (List.map ppPattern ps)
        ConsP ",," ps ->
            parens <|
                join (string ",") (List.map ppPattern ps)

        ConsP ":" [p1,p2] ->
            parens <|
                (ppPattern p1
                       |> a (taggedString ":" Constructor)
                       |> a (ppPattern p2))
        ConsP tag [] ->
            taggedString tag Constructor
        ConsP tag ps ->
            parens <|
                words <| (taggedString tag Constructor :: List.map ppPattern ps)
        AsP var pat -> 
            taggedString var Variable
                |> a (string "@")
                |> a (ppPattern pat)


-- check if an cons expression has an evaluated spine
-- in that case return the list of expressions
checkSpine : Expr -> Maybe (List Expr)
checkSpine e
    = case e of
          Cons _ "[]" [] ->
              Just []
          Cons _ ":" [hd,tl] ->
                      checkSpine tl
                      |> Maybe.andThen (\l -> Just (hd::l))
          _ ->
              Nothing

-- check if a list of expressions consists only of characters
checkChars : List Expr -> Maybe String
checkChars es =
    if List.all isChar es then
        Just (String.concat (List.map getChar es))
    else
        Nothing

isChar : Expr -> Bool
isChar e =
    case e of
        Char _ -> True
        _ -> False
            
getChar : Expr -> String
getChar e =
    case e of
        Char c -> escapeChar '"' c
        _ -> ""   -- this should not happen!

    
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


-- an HTML renderer;
-- use CodeMirror CSS tags for consistency
htmlRenderer : Renderer Tag (List (Html msg)) (Html msg)
htmlRenderer =
    { outer = (\elems -> Html.span [class "cm-s-default"] (List.reverse elems))
    , init = []
    , tagged = htmlTagged
    , untagged = htmlUntagged
    , newline = (\elems -> Html.br [] []::elems)
    }

htmlTagged : Tag -> String -> List (Html msg) -> List (Html msg)
htmlTagged tag str lst =
    (Html.span (tagToAttributes tag) [Html.text str]) :: lst

htmlUntagged : String -> List (Html msg) -> List (Html msg)
htmlUntagged str lst
    = (Html.text (replaceSpaces str)) :: lst

-- replace spaces with unbreakable ones
replaceSpaces : String -> String
replaceSpaces
    = String.map (\x -> if x==' ' then '\u{00a0}' else x)
      
tagToAttributes : Tag -> List (Attribute msg)
tagToAttributes tag
    = case tag of
          Keyword ->
              [class "cm-keyword"]
          Variable ->
              [class "cm-variable"]
          Constructor ->
              [class "cm-variable-2"]
          Literal ->
              [class "cm-number"]
          String ->
              [class "cm-string"]
          Linenumber ->
              [class "linenumber"]
          Exception ->
              [class "exception"]
