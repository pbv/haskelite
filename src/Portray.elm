{-
  Pretty-printer for Haskelite expressions, types and machine configurations
  Pedro Vasconcelos 2021--24
-}
module Portray exposing
    (PrettyCfg, ppExpr, ppPattern, ppType, ppConfStep, htmlRenderer)

import AST exposing (Expr(..), Matching(..), Qual(..), 
    Info, Bind, Pattern(..), Name)

import Types exposing (Type(..))

import Machine.Types exposing (..)
import Machine.Heap as Heap
import Pretty exposing
    (Doc, empty, string, taggedString, char, space, parens, brackets,
     a, words, nest, indent, align, hang, group, join)
import Pretty.Renderer exposing (Renderer)

import Html exposing (Html, Attribute)
import Html.Attributes exposing (class)

-- precedence and depth for pretty printing
type alias Prec = Int

type alias Depth = Int    

-- tags for expression highlighting
type Tag = Keyword
         | Literal
         | String
         | Constructor
         | Variable
         | Linenumber
         | Exception
         | BeginMark
         | EndMark  
    

                  
type alias PrettyCtx
    = { prettyLists : Bool
      , prettyEnums : Bool
      -- combinators that depend on layout setting
      , line : Doc Tag
      , softline : Doc Tag
      }

type alias PrettyCfg a
    = { a | prettyLists : Bool
      , prettyEnums : Bool
      , layout : Bool
      , columns : Int
      }
    
makeContext : PrettyCfg a -> PrettyCtx
makeContext cfg
    = { prettyLists = cfg.prettyLists
      , prettyEnums = cfg.prettyEnums
      , line = if cfg.layout then Pretty.line else Pretty.space
      , softline = if cfg.layout then Pretty.softline else Pretty.space
      }
   

listCons : Doc Tag
listCons = taggedString ":" Constructor
           
-- conditionally add parenthesis to a document
parensIf : Bool -> Doc t -> Doc t
parensIf b doc
    = if b then Pretty.parens doc else doc


                  
-- pretty print a configuration step
ppConfStep : PrettyCfg a -> Bool -> Int -> Conf -> Maybe (Doc Tag)
ppConfStep cfg highlight step conf 
    = ppConf (makeContext cfg) highlight conf |> Maybe.andThen
          (\doc -> Just (if step>0 then 
                             taggedString "=" Linenumber
                                |> a space
                                |> a (align doc)
                         else
                             space |> a space |> a (align doc)))

                  
                 
-- pretty print a configuration
ppConf :  PrettyCtx -> Bool -> Conf -> Maybe (Doc Tag)
ppConf ctx highlight (heap, control, stack)
    = case getExpr control of
         Just expr ->
             case unwindStack stack (if highlight then Marked expr
                                     else expr) of
                 (stk,expr1) ->
                     let ellipsis = String.repeat (List.length stk) "."
                         expr2 = Heap.expandExpr heap expr1
                     in Just (taggedString ellipsis Linenumber
                             |> a space     
                             |> a (align (ppExpr ctx 0 0 expr2)))

         _ ->
             Nothing

-- convert the stack into a partial expression;
-- aborts if we reach a guard/case alternative
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
          (DeepEval::rest) ->
              unwindStack rest acc
          (Continue expr ctx::rest) ->
              unwindStack rest (ctx.set acc expr)
          (MatchEnd::rest) ->
              unwindStack rest acc              
          _ ->
              (stack, acc)


                 
getExpr : Control -> Maybe Expr
getExpr ctrl
    = case ctrl of
          E expr ->
              Just expr
          M (Return expr _) [] ->
              Just expr
          _ ->
              Nothing
                 
               
-- pretty-print an expression 
ppExpr : PrettyCtx -> Prec -> Depth -> Expr -> Doc Tag
ppExpr ctx prec depth e 
    = case e of 
        Number n ->
            parensIf (prec>0 && n<0) <|
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
                            ppList ctx depth args
                    else
                        ppListCons ctx prec depth (args++[last])
               else
                   ppListCons ctx prec depth (args++[last])

        Cons _ tag args ->
            if tag == "," || tag == ",," || tag == ",,," then
                let sep = if depth>0 then empty else ctx.softline
                in 
                group <| parens <| join (char ',' |> a sep) <|
                    List.map (ppExpr ctx 0 (1+depth)) args
            else
                ppGenericCons ctx prec depth tag args

        BinaryOp op e1 e2 ->
            let sep = if depth>0 then empty else ctx.softline
            in 
            parensIf (prec>0) 
                 (ppExpr ctx 1 depth e1
                     |> a sep
                     |> a (string (formatOperator op))
                     |> a sep
                     |> a (ppExpr ctx 1 depth e2))

        UnaryOp op e1 ->
            parensIf (prec>0) 
                (string op |> a space |> a (ppExpr ctx 1 depth e1))

        App e0 e1 ->
            ppApp ctx prec depth e0 e1
                
        Lam _ optid match ->
            case collectMatchingArgs match [] of
                (_, []) ->
                    ppLambda ctx optid match
                (match1, args1) ->
                    ppApp1 ctx prec depth (AST.lambda optid match1) args1

        Let binds e1 ->
             parensIf (prec>0)
                 (group 
                      (taggedString "let" Keyword
                      |> a space
                      |> a (nest 4 (ppBinds ctx depth binds))
                      |> a ctx.line
                      |> a (taggedString "in" Keyword)
                      |> a space
                      |> a (ppExpr ctx 0 depth e1)))
                      
     
        Case expr alts ->
            parensIf (prec>0) 
                (group (hang 4 (taggedString "case" Keyword
                    |> a space
                    |> a (ppExpr ctx 1 depth expr)
                    |> a space 
                    |> a (taggedString "of" Keyword)
                    |> a ctx.line 
                    |> a (ppAlts ctx depth alts))))
                    
        IfThenElse e1 e2 e3 ->
            parensIf (prec>0)
                (group (taggedString "if" Keyword
                        |> a space
                        |> a (ppExpr ctx 0 depth e1)
                        |> a space
                        |> a (taggedString "then" Keyword)
                        |> a (nest 4 (ctx.line |>
                                       a (ppExpr ctx 0 depth e2)))
                        |> a ctx.line 
                        |> a (taggedString "else" Keyword)
                        |> a (nest 4 (ctx.line |>
                                       a (ppExpr ctx 0 depth e3)))))

        AST.Exception msg ->
            taggedString ("exception: " ++ msg) Exception

        AST.ListComp e1 qs ->
            ppListComp ctx depth e1 qs

        AST.Unimplemented na ->
            string na.source

        AST.Marked e1 ->
            taggedString "(" BeginMark
                |> a (ppExpr ctx prec depth e1)
                |> a (taggedString ")" EndMark)   
        

-- list constructor
ppListCons : PrettyCtx -> Prec -> Depth -> List Expr -> Doc Tag
ppListCons ctx prec depth args 
    = let sep = if depth>0 then empty else ctx.softline
      in 
      parensIf (prec>0) <| group <|
       join (sep |> a listCons |> a sep) <|
          List.map (ppExpr ctx 1 (1+depth)) args

                
-- general constructor
ppGenericCons : PrettyCtx -> Prec -> Depth -> String -> List Expr -> Doc Tag
ppGenericCons ctx prec depth cons args
    = case args of
          [] ->
              taggedString cons Constructor
          [arg] ->
              parensIf (prec>0)
                  (taggedString cons Constructor
                      |> a space 
                      |> a (ppExpr ctx 1 depth arg))
          _ ->
              let docs =
                      List.map (ppExpr ctx 1 (1+depth)) args
               in
                   parensIf (prec>0)
                       (group <| hang 2 <| join ctx.line
                            (taggedString cons Constructor::docs))


                      
ppList : PrettyCtx -> Depth -> List Expr -> Doc Tag
ppList ctx depth es
    = let sep = if depth>1 then empty else ctx.softline
      in  group <| brackets <|
            join (char ',' |> a sep) <|
                List.map (ppExpr ctx 0 (1+depth)) es

              

ppListComp : PrettyCtx -> Depth -> Expr -> List Qual -> Doc Tag
ppListComp ctx depth e qs
    = group
      (brackets
        (ppExpr ctx 0 depth e
                 |> a space
                 |> a (char '|')
                 |> a space
                 |> a (join (char ',') (List.map (ppListQual ctx depth) qs))))

ppListQual : PrettyCtx -> Depth -> Qual -> Doc Tag
ppListQual ctx depth qs
    = case qs of
          (Gen p e) ->
             ppPattern p
                 |> a (string "<-")
                 |> a (ppExpr ctx 0 depth e)
          (Guard e) ->
              ppExpr ctx 0 depth e
          (LetQual x e) ->
              taggedString "let" Keyword
                  |> a space
                  |> a (taggedString x Variable)
                  |> a space
                  |> a (string "=")
                  |> a space
                  |> a (ppExpr ctx 0 depth e)
                        
      
-- pretty print a generic application
ppApp : PrettyCtx -> Prec -> Depth -> Expr -> Expr -> Doc Tag
ppApp ctx prec depth e0 e1
    = let (fun, args) = collectArgs e0 [e1]
      in if ctx.prettyEnums then
             ppApp1Enums ctx prec depth fun args
         else
             ppApp1 ctx prec depth fun args

-- used when pretty printing enums
ppApp1Enums : PrettyCtx -> Prec -> Depth -> Expr -> List Expr -> Doc Tag
ppApp1Enums ctx prec depth fun args
    = let depth1 = 1+depth
      in case (nameOf fun,args) of
          -- special cases for enums
          (Just "enumFrom", [e1]) ->
              brackets (ppExpr ctx 0 depth1 e1 |> a (string ".."))
          (Just "enumFromTo", [e1,e2]) ->
              brackets 
                   (ppExpr ctx 0 depth1 e1
                         |> a (string "..")
                         |> a (ppExpr ctx 0 depth1 e2))
          (Just "enumFromThen", [e1,e2]) ->
              brackets  
                  (ppExpr ctx 0 depth1 e1
                           |> a (char ',')
                           |> a (ppExpr ctx 0 depth1 e2)
                           |> a (string ".."))                  
          (Just "enumFromThenTo", [e1,e2,e3]) ->
              brackets 
                  (ppExpr ctx 0 depth1 e1
                         |> a (char ',')
                         |> a (ppExpr ctx 0 depth1 e2)
                         |> a (string "..")
                         |> a (ppExpr ctx 0 depth1 e3))
  
          -- special cases for binary operators
          (Just op, [arg1, arg2]) ->
              if AST.isOperator op then
                  ppExpr ctx prec depth (BinaryOp op arg1 arg2)
              else
                  ppApp2 ctx prec depth fun args
          _ ->
              ppApp2 ctx prec depth fun args
              
-- used when not pretty printing enums          
ppApp1 : PrettyCtx -> Prec -> Depth -> Expr -> List Expr -> Doc Tag
ppApp1 ctx prec depth fun args
    = let depth1 = 1 + depth
      in case (nameOf fun,args) of
          -- special cases for binary operators
          (Just op, [arg1, arg2]) ->
              if AST.isOperator op then
                  ppExpr ctx prec depth1 (BinaryOp op arg1 arg2)
              else
                  ppApp2 ctx prec depth1 fun args
          _ ->
              ppApp2 ctx prec depth1 fun args

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
                  
ppApp2 : PrettyCtx -> Prec -> Depth -> Expr -> List Expr -> Doc Tag
ppApp2 ctx prec depth fun args
    = parensIf (prec>0) <|
      (ppExpr ctx 0 depth fun
        |> a space
        |> a (words (List.map (ppExpr ctx 1 depth) args)))
                 
                  
              
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
ppAlts : PrettyCtx -> Depth -> List (Pattern,Expr,Info) -> Doc Tag
ppAlts ctx depth alts
    = join (char ';' |> a ctx.line) (List.map (ppAlt ctx depth) alts)

ppAlt : PrettyCtx -> Depth -> (Pattern,Expr,Info) -> Doc Tag
ppAlt ctx depth (patt, expr, info)
    = ppPattern patt
          |> a (string " -> ")
          |> a (ppExpr ctx 0 0 expr)


-- pretty print a lambda
ppLambda : PrettyCtx  -> Maybe Name -> Matching -> Doc Tag
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
                      parens (char '\\' |> a (ppMatching ctx 0 m))
                  Return e _ ->
                      ppExpr ctx 0 0 e
                  _ ->
                      taggedString "<unimplemented>" Exception

          
-- pretty print a matching            
ppMatching : PrettyCtx -> Depth -> Matching -> Doc Tag
ppMatching ctx depth m =
    case m of
        (Match p m1) ->
            ppPattern p
                 |> a space
                 |> a (ppMatching ctx depth m1)
        (Return e _) ->
            string "->"
                  |> a space
                  |> a (ppExpr ctx 0 depth e)

        _ ->
            string "<unimplemented>"


-- pretty print a list of bindings
ppBinds : PrettyCtx -> Depth -> List Bind -> Doc Tag
ppBinds ctx depth binds
    = join (char ';' |> a ctx.line) (List.map (ppBind ctx depth) binds)

ppBind : PrettyCtx -> Depth -> Bind -> Doc Tag
ppBind ctx depth bind
    = string bind.name
          |> a space
          |> a (char '=') 
          |> a space 
          |> a (ppExpr ctx 0 depth (removeName bind.name bind.expr))


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
        ConsP ",,," ps ->
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

-- accumulator for HTML  
type HtmlAcc msg
    = Outside (List (Html msg))                  -- outside of a mark
    | Inside (List (Html msg)) (List (Html msg)) -- inside of a mark


htmlRenderer : Renderer Tag (HtmlAcc msg) (Html msg)
htmlRenderer =
    { outer = htmlOuter
    , init = Outside []
    , tagged = htmlTagged
    , untagged = htmlUntagged
    , newline = htmlNewline
    }

htmlOuter : HtmlAcc msg -> Html msg
htmlOuter acc =
    case acc of
        Outside outelems ->
            Html.span [] (List.reverse outelems)
        Inside inelems outelems ->
            Html.span []
                [ Html.span [class "marked"] (List.reverse inelems)
                , Html.span [] (List.reverse outelems) ]            

htmlNewline : HtmlAcc msg -> HtmlAcc msg
htmlNewline acc 
    = let new = Html.br [] []
      in 
          case acc of
              Outside elems ->
                  Outside (new::elems)
              Inside inelems outelems ->
                  Inside (new::inelems) outelems

    
htmlTagged : Tag -> String -> HtmlAcc msg -> HtmlAcc msg
htmlTagged tag str acc =
    case (tag, acc) of
        (BeginMark, Outside elems) ->
            Inside [] elems
        (EndMark, Inside inelems outelems) ->
            let mark = Html.span [class "marked"] (List.reverse inelems)
            in Outside (mark::outelems)
        (_, Outside elems) ->
            let new = Html.span (tagToAttributes tag) [Html.text str]
            in Outside (new::elems)
        (_, Inside inelems outelems) ->
            let new = Html.span (tagToAttributes tag) [Html.text str]
            in Inside (new::inelems) outelems

htmlUntagged : String -> HtmlAcc msg -> HtmlAcc msg
htmlUntagged str acc
    = let new = Html.text (replaceSpaces str)
      in case acc of
          Outside elems ->
              Outside (new :: elems)
          Inside inelems outelems ->
              Inside (new::inelems) outelems

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
          BeginMark ->
              []
          EndMark ->
              []

