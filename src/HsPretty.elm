{-
  Pretty-printer for Haskelite expressions, types and machine configurations
  Pedro Vasconcelos 2021--24
-}
module HsPretty exposing
    (Options, showExpr, showPattern, showType, htmlConfStep)

import AST exposing
    (Expr(..), Matching(..), Qual(..), Info, Bind, Pattern(..), Name)
import Types exposing (Type(..))

import Machine.Types exposing (..)
import Machine.Heap as Heap
import Pretty exposing
    (Doc, empty, string, taggedString, char, space, parens, brackets,
     a, words, nest, indent, align, hang, group, join)
import Pretty.Renderer exposing (Renderer)

import Html exposing (Html, Attribute)
import Html.Attributes exposing (class)
   
type alias Options r
    = { r
      | prettyLists : Bool     -- should we prettify lists?
      , prettyEnums : Bool     -- should we prettify prelude enum functions?
      , layout : Bool          -- should we use layout?
      , columns : Int          -- number of columns for layout
      }

-- very high columns for pretty-printint without layout
infiniteLength : Int
infiniteLength
    = 1000000
    

type alias Prec = Int
              
type alias PrettyCtx
    = { prec : Prec          -- precedence level for placing parenthesis
      , depth : Int          -- depth level (>=0) for tweaking printing
      , prettyLists : Bool
      , prettyEnums : Bool
      -- combinators that depend on layout setting
      , line : Doc Tag
      , softline : Doc Tag
      }

makeCtx : Options r -> PrettyCtx
makeCtx opts 
    = { prec = 0
      , depth = 0
      , prettyLists = opts.prettyLists
      , prettyEnums = opts.prettyEnums
      , line = if opts.layout then Pretty.line else Pretty.space
      , softline = if opts.layout then Pretty.softline else Pretty.space
      }
   

listCons : Doc Tag
listCons = taggedString ":" Constructor
           
-- conditionally add parenthesis to a document
parensIf : Bool -> Doc t -> Doc t
parensIf b doc
    = if b then Pretty.parens doc else doc
              
-- tags for expression highlighting
type Tag = Keyword
         | Literal
         | String
         | Constructor
         | Variable
         | Linenumber
         | Exception
        
--
-- top-level functions to render to a string
--
showType : Type -> String
showType ty
    = Pretty.pretty infiniteLength (ppType 0 ty)
      
showExpr : Expr -> String
showExpr e
    = let
        opts = { prettyLists = True
               , prettyEnums = True
               , layout = False
               , columns = infiniteLength
               }
      in Pretty.pretty infiniteLength (ppExpr (makeCtx opts) e)

showPattern : Pattern -> String
showPattern p
    = Pretty.pretty infiniteLength (ppPattern p)


--
-- render a configuration to HTML
--
htmlConfStep : Options t -> Int -> Conf -> Maybe (Html msg)
htmlConfStep opts step conf 
    -- compute an approximate number of columns for layout
    = case ppConfStep opts step conf of
          Just doc ->
              Just <| Pretty.Renderer.pretty opts.columns htmlRenderer doc
          Nothing ->
              Nothing

      
-- pretty print a configuration step
ppConfStep : Options t -> Int -> Conf -> Maybe (Doc Tag)
ppConfStep opts step conf 
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
ppConf : Options t -> Conf -> Maybe (Doc Tag)
ppConf opts (heap, control, stack)
    = case (getExpr control) of
         Just expr ->
             let ctx = makeCtx opts 
             in 
             case unwindStack stack expr of
                 ([], expr1) ->
                     let expr2 = Heap.expandExpr heap expr1
                     in Just (ppExpr ctx expr2)
                 (stk,expr1) ->
                     let ellipsis = String.repeat (List.length stk) "."
                         expr2 = Heap.expandExpr heap expr1
                     in Just (taggedString ellipsis Linenumber
                             |> a space     
                             |> a (align (ppExpr ctx expr2)))
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
                 
               
-- pretty-print an expression 
ppExpr : PrettyCtx -> Expr -> Doc Tag
ppExpr ctx e 
    = case e of 
        Number n ->
            parensIf (ctx.prec>0 && n<0) <|
                taggedString (String.fromInt n) Literal
        Char c ->
            taggedString (prettyChar c) String

        Var x ->
            if AST.isOperator x then
                parens (string x)
            else if Heap.isIndirection x then
                     taggedString x Exception
                 else
                     taggedString x Variable
              
        Cons _ ":" _ ->
            let (args, last) = getSpine e
            in if ctx.prettyLists then
                if isNil last then
                    case checkChars args of
                        Just s ->
                            taggedString ("\"" ++ s ++ "\"") String
                        Nothing ->
                            ppList ctx args
                    else
                        ppListCons ctx (args++[last])
               else
                   ppListCons ctx (args++[last])

        Cons _ tag args ->
            if tag == "," || tag == ",," || tag == ",,," then
                let sep = if ctx.depth>0 then empty else ctx.softline
                in 
                group <| parens <| join (char ',' |> a sep) <|
                    List.map (ppExpr {ctx|prec=0,depth=1+ctx.depth}) args
            else
                ppGenericCons ctx tag args

        BinaryOp op e1 e2 ->
            let sep = if ctx.depth>0 then empty else ctx.softline
            in 
            parensIf (ctx.prec>0) 
                 (ppExpr {ctx|prec=1} e1
                     |> a sep
                     |> a (string (formatOperator op))
                     |> a sep
                     |> a (ppExpr {ctx|prec=1} e2))

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
                      |> a space
                      |> a (nest 4 (ppBinds ctx binds))
                      |> a ctx.line
                      |> a (taggedString "in" Keyword)
                      |> a space
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
                        |> a (nest 4 (ctx.line |>
                                       a (ppExpr {ctx|prec=0} e2)))
                        |> a ctx.line 
                        |> a (taggedString "else" Keyword)
                        |> a (nest 4 (ctx.line |>
                                       a (ppExpr {ctx|prec=0} e3)))))

        AST.Exception msg ->
            taggedString ("exception: " ++ msg) Exception

        AST.ListComp e1 qs ->
            ppListComp ctx e1 qs

        AST.Unimplemented na ->
            string na.source

-- list constructor
ppListCons : PrettyCtx -> List Expr -> Doc Tag
ppListCons ctx args 
    = let sep = if ctx.depth>0 then empty else ctx.softline
      in 
      parensIf (ctx.prec>0) <| group <|
       join (sep |> a listCons |> a sep) <|
          List.map (ppExpr {ctx|prec=1,depth=1+ctx.depth}) args

                
-- general constructor
ppGenericCons : PrettyCtx -> String -> List Expr -> Doc Tag
ppGenericCons ctx cons args
    = case args of
          [] ->
              taggedString cons Constructor
          [arg] ->
              parensIf (ctx.prec>0)
                  (taggedString cons Constructor
                      |> a space 
                      |> a (ppExpr {ctx|prec=1} arg))
          _ ->
              let docs =
                      List.map (ppExpr {ctx|prec=1,depth=1+ctx.depth}) args
               in
                   parensIf (ctx.prec>0)
                       (group <| hang 2 <| join ctx.line
                            (taggedString cons Constructor::docs))


                      
ppList : PrettyCtx -> List Expr -> Doc Tag
ppList ctx es
    = let sep = if ctx.depth>1 then empty else ctx.softline
      in  group <| brackets <|
            join (char ',' |> a sep) <|
                List.map (ppExpr {ctx|depth=1+ctx.depth}) es

              

ppListComp : PrettyCtx -> Expr -> List Qual -> Doc Tag
ppListComp ctx e qs
    = let ctx0 = {ctx|prec=0}
      in 
          group (brackets (ppExpr ctx0 e
                          |> a space
                          |> a (char '|')
                          |> a space
                          |> a (join (char ',') (List.map (ppListQual ctx0) qs))))

ppListQual : PrettyCtx -> Qual -> Doc Tag
ppListQual ctx qs
    = case qs of
          (Gen p e) ->
             ppPattern p
                 |> a (string "<-")
                 |> a (ppExpr ctx e)
          (Guard e) ->
              ppExpr ctx e
          (LetQual x e) ->
              taggedString "let" Keyword
                  |> a space
                  |> a (taggedString x Variable)
                  |> a space
                  |> a (string "=")
                  |> a space
                  |> a (ppExpr ctx e)
                        
      
-- pretty print a generic application
ppApp : PrettyCtx -> Expr -> Expr -> Doc Tag
ppApp ctx e0 e1
    = let (fun, args) = collectArgs e0 [e1]
      in if ctx.prettyEnums then
             ppApp1Enums ctx fun args
         else
             ppApp1 ctx fun args

-- used when pretty printing enums
ppApp1Enums : PrettyCtx -> Expr -> List Expr -> Doc Tag
ppApp1Enums ctx fun args
    = let ctx0 = {ctx|prec=0,depth=1+ctx.depth}
      in case (nameOf fun,args) of
          -- special cases for enums
          (Just "enumFrom", [e1]) ->
              brackets (ppExpr ctx0 e1 |> a (string ".."))
          (Just "enumFromTo", [e1,e2]) ->
              brackets 
                   (ppExpr ctx0 e1
                         |> a (string "..")
                         |> a (ppExpr ctx0 e2))
          (Just "enumFromThen", [e1,e2]) ->
              brackets  
                  (ppExpr ctx0 e1
                           |> a (char ',')
                           |> a (ppExpr ctx0 e2)
                           |> a (string ".."))                  
          (Just "enumFromThenTo", [e1,e2,e3]) ->
              brackets 
                  (ppExpr ctx0 e1
                         |> a (char ',')
                         |> a (ppExpr ctx0 e2)
                         |> a (string "..")
                         |> a (ppExpr ctx0 e3))
  
          -- special cases for binary operators
          (Just op, [arg1, arg2]) ->
              if AST.isOperator op then
                  ppExpr ctx (BinaryOp op arg1 arg2)
              else
                  ppApp2 ctx fun args
          _ ->
              ppApp2 ctx fun args
              
-- used when not pretty printing enums          
ppApp1 : PrettyCtx -> Expr -> List Expr -> Doc Tag
ppApp1 ctx fun args
    = let ctx1 = {ctx|depth=1+ctx.depth}
      in case (nameOf fun,args) of
          -- special cases for binary operators
          (Just op, [arg1, arg2]) ->
              if AST.isOperator op then
                  ppExpr ctx1 (BinaryOp op arg1 arg2)
              else
                  ppApp2 ctx1 fun args
          _ ->
              ppApp2 ctx1 fun args

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
      (ppExpr {ctx|prec=0} fun
        |> a space
        |> a (words (List.map (ppExpr {ctx|prec=1}) args)))
                 
                  
              
-- auxiliary function to collect arguments to an application
collectArgs : Expr -> List Expr -> (Expr, List Expr)
collectArgs e acc
    = case e of                         
          App e1 arg ->
              collectArgs e1 (arg::acc)
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
    = let ctx0 = {ctx|prec=0}
      in join (char ';' |> a ctx.line) (List.map (ppAlt ctx0) alts)

ppAlt : PrettyCtx -> (Pattern,Expr,Info) -> Doc Tag
ppAlt ctx (patt, expr, info)
    = ppPattern patt
          |> a (string " -> ")
          |> a (ppExpr ctx expr)


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
    = string bind.name
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
    = if AST.isOperator op then op else "`"++op++"`"
                

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
                  Pretty.join (char ',') (List.map (ppType 0) ts) 
          TyConst "(,,)" ts ->
              parens <|
                  Pretty.join (char ',') (List.map (ppType 0) ts) 
          TyConst "(,,,)" ts ->
              parens <|
                  Pretty.join (char ',') (List.map (ppType 0) ts) 
              
          TyConst c ts ->
              parensIf (prec>0) 
                  (words (string c :: List.map (ppType 1) ts))
                      
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
            parens <| join (char ',') (List.map ppPattern ps)
        ConsP ",," ps ->
            parens <| join (char ',') (List.map ppPattern ps)

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
                |> a (char '@')
                |> a (ppPattern pat)



-- get all expressions along the spine of a cons-list
getSpine : Expr -> (List Expr, Expr)
getSpine expr
    = let go e acc
              = case e of
                    Cons _ ":" [e1,e2] -> go e2 (e1::acc)
                    _ -> (List.reverse acc, e)
      in go expr []

isNil : Expr -> Bool
isNil e
    = case e of
          Cons _ "[]" [] ->
              True
          _ ->
              False
                  
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

