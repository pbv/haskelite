{-
  Haskelite, a single-step interpreter for a subset of Haskell.
  Main module exporting an interactive HTML entity
  Pedro Vasconcelos, 2021
-}
module Haskelite exposing (..)

import AST exposing (Expr(..), Decl, Name)
import HsParser
import Eval exposing (Functions)
import Parser
import Pretty
import Prelude
import Context exposing (Context)
import Monocle.Optional as Monocle
import Html exposing (..)
import Html.Attributes exposing (value, class, placeholder, disabled,
                                     size, rows, cols, spellcheck)
import Html.Events exposing (on, onClick, onInput)
import Platform.Cmd as Cmd
import Platform.Sub as Sub

import List.Extra as List
import Dict exposing (Dict)
import Browser

type alias Model
    = { expression : Expr             -- current expression
      , previous : List (Expr, String) -- list of previous steps
      , functions : Functions
      , inputExpr : String
      , outputExpr : Result String Expr
      , inputDecls : String
      , outputDecls : Result String (List Decl)
      , mode : Mode
      }

type Mode
    = Editing | Reducing 

type Msg
    = Step Context    -- single step evaluation at a context
    | Previous        -- undo one evaluation step
    | Next            -- next outermost step
    | Reset           -- reset evaluation
    | EditExpr String   -- edit expression
    | EditDecls String  -- edit declarations
    | Edit              -- go into editing mode
    | SaveEdits         -- go into reduction mode


-- the main entry point for the app
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

      
-- * inicializing the application
init : {expression:String, declarations:String} -> (Model, Cmd msg) 
init config  =
    let outputExpr = Result.mapError Pretty.deadEndsToString
                     <| Parser.run HsParser.topExprEnd config.expression
        outputDecls = Result.mapError Pretty.deadEndsToString
                      <| Parser.run HsParser.declListEnd config.declarations
    in case (outputExpr, outputDecls) of
           (Ok expr, Ok decls) ->
               ({ expression = expr
                , previous = []
                , functions = Eval.collectFunctions decls Prelude.functions
                , inputExpr = config.expression
                , outputExpr = outputExpr
                , inputDecls = config.declarations
                , outputDecls = outputDecls
                , mode = Reducing
                }
               , Cmd.none)
           _ ->
               ({ expression = Fail "syntax"
                , previous = []
                , functions = Prelude.functions
                , inputExpr = config.expression
                , outputExpr = outputExpr
                , inputDecls = config.declarations
                , outputDecls = outputDecls
                , mode = Editing
                }
               , Cmd.none)

                 

        
view : Model -> Html Msg
view model =
    case model.mode of
        Editing -> editingView model
        Reducing -> reduceView model

editingView : Model -> Html Msg
editingView model =
    div [] [ button [ class "navbar", onClick SaveEdits ] [ text "Save" ]
           , br [] []
           , input [ placeholder "Enter an expression"
                   , value model.inputExpr
                   , size 80
                   , spellcheck False
                   , class "editline"
                   , onInput EditExpr
                   ]  []
           , br [] []
           , case model.outputExpr of
                 Err msg -> span [class "error"] [text msg]
                 _ -> span [] []
           , br [] []
           , textarea [ value model.inputDecls
                      , rows 24
                      , cols 80
                      , spellcheck False
                      , onInput EditDecls
                      ] []
           , br [] []
           , case model.outputDecls of
                 Err msg -> span [class "error"] [text msg]
                 _ -> span [] []
           ]

reduceView : Model -> Html Msg
reduceView model =
    div []
        [ span [] [ button [ class "navbar"
                           , disabled (not (List.isEmpty model.previous))
                           , onClick Edit] [text "Edit"]
                  , button [ class "navbar"
                           , onClick Reset
                           ] [text "Reset"]
                  , button [ class "navbar"
                           , disabled (List.isEmpty model.previous)
                           , onClick Previous] [text "< Prev"]
                  , button [ class "navbar"
                           , disabled (isNormalForm model.functions model.expression)
                           , onClick Next] [text "Next >"]
                  ]
        , div [class "lines"]
             <| List.map lineView (List.reverse model.previous) ++
                 [ div [class "current"]
                          [ renderExpr
                                model.functions model.expression Context.hole ]
                 ]
        ]
      
lineView : (Expr, String) -> Html Msg
lineView (expr, info) =
    div [class "line"]
        [ text (Pretty.prettyExpr expr)
        , span [class "info"] [ text info ]
        ]
        


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case model.mode of
        Reducing ->
            (reduceUpdate msg model, Cmd.none)
        Editing ->
            (editUpdate msg model, Cmd.none)

        
reduceUpdate : Msg -> Model -> Model
reduceUpdate msg model =
    case msg of
        Step ctx ->
            case Eval.redexCtx model.functions model.expression ctx of
                Just (newExpr, info) ->
                    { model | expression = newExpr
                    , previous = (model.expression, info) :: model.previous
                    }
                
                Nothing ->
                    model

        Previous ->
            case model.previous of
                ((oldExpr, info) :: rest) ->
                    { model | expression = oldExpr, previous = rest }
                [] ->
                    model

        Next ->
            case reduceNext model.functions model.expression of
                Just (newExpr, info) ->
                    { model | expression = newExpr
                    , previous = (model.expression, info) :: model.previous
                    }
                Nothing ->
                    model
                        
        Reset ->
            case List.last model.previous of
                Just (expr,_) ->
                    { model | expression = expr, previous = [] }
                Nothing ->
                    model
        Edit ->
            { model | mode = Editing }
            
        _ ->
            model


reduceNext : Functions -> Expr -> Maybe (Expr,String)
reduceNext functions expr
    = Eval.outermostRedex functions expr
        |> Maybe.andThen (\ctx -> Eval.redexCtx functions expr ctx)

           
isNormalForm : Functions -> Expr -> Bool
isNormalForm functions expr
    = case reduceNext functions expr of
          Just _ -> False
          Nothing -> True
           
                
editUpdate : Msg -> Model -> Model
editUpdate msg model =
    case msg of
        EditExpr string ->
            let
                output = Result.mapError Pretty.deadEndsToString
                         <| Parser.run HsParser.topExprEnd string
            in
                { model | inputExpr = string, outputExpr = output }

        EditDecls string ->
            let
                output = Result.mapError Pretty.deadEndsToString
                         <| Parser.run HsParser.declListEnd string
            in
                { model | inputDecls = string, outputDecls = output }
        
        SaveEdits ->
            case (model.outputExpr, model.outputDecls) of
                (Ok expr, Ok decls) ->
                    { model | expression = expr
                    , previous = []
                    , functions = Eval.collectFunctions decls Prelude.functions
                    , mode = Reducing
                    }
                _ ->
                    model 
        _ ->
            model
                       
subscriptions : Model -> Sub msg
subscriptions _ = Sub.none

                 
-- render an interactive expression; toplevel function
renderExpr : Functions -> Expr -> Context -> Html Msg
renderExpr functions expr ctx =
    renderExpr_ 0 functions expr ctx

-- worker function 
renderExpr_ : Int -> Functions -> Expr -> Context -> Html Msg
renderExpr_ prec functions expr ctx 
    = case expr of
          Var x ->
              redexSpan functions expr ctx 
                   [ text <| if Pretty.isOperator x then "("++x++")" else x ]
                  
                  
          Number n ->
              text (String.fromInt n)

          Boolean b ->
              text <| if b then "True" else "False"

          TupleLit args ->
              let
                  n = List.length args - 1
                  ctxs = List.map (\i -> Monocle.compose ctx (Context.tupleItem i))
                          (List.range 0 n)
                  items = List.intersperse (text ",")
                          <| List.map2 (renderExpr functions) args ctxs
              in
                  span [] <| (text "(" :: items) ++ [text ")"]
                  
                              
          ListLit args ->
              let
                  n = List.length args - 1
                  ctxs = List.map (\i -> Monocle.compose ctx (Context.listItem i))
                          (List.range 0 n)
                  items = List.intersperse (text ",")
                          <| List.map2 (renderExpr functions) args ctxs
              in
                  span [] <| (text "[" :: items) ++ [text "]"]

          EnumFrom e0 ->
              let
                  ctx0 = Monocle.compose ctx Context.enumFrom0
              in
                  span []
                      [ text "["
                      , renderExpr_ 1 functions e0 ctx0
                      , redexSpan functions expr ctx [text ".."]
                      , text "]"
                      ]

          EnumFromThen e0 e1 ->
              let
                  ctx0 = Monocle.compose ctx Context.enumFromThen0
                  ctx1 = Monocle.compose ctx Context.enumFromThen1
              in
                  span []
                      [ text "["
                      , renderExpr_ 1 functions e0 ctx0
                      , text ","
                      , renderExpr_ 1 functions e1 ctx1
                      , redexSpan functions expr ctx [text ".."]
                      , text "]"
                      ]
                      
          EnumFromTo e0 e1 ->
              let
                  ctx0 = Monocle.compose ctx Context.enumFromTo0
                  ctx1 = Monocle.compose ctx Context.enumFromTo1
              in
                  span []
                      [ text "["
                      , renderExpr_ 1 functions e0 ctx0
                      , redexSpan functions expr ctx [text ".."]
                      , renderExpr_ 1 functions e1 ctx1
                      , text "]"
                      ]

          EnumFromThenTo e0 e1 e2 ->
              let
                  ctx0 = Monocle.compose ctx Context.enumFromThenTo0
                  ctx1 = Monocle.compose ctx Context.enumFromThenTo1
                  ctx2 = Monocle.compose ctx Context.enumFromThenTo2
              in
                  span []
                      [ text "["
                      , renderExpr_ 1 functions e0 ctx0
                      , text ","
                      , renderExpr_ 1 functions e1 ctx1
                      , redexSpan functions expr ctx [text ".."]
                      , renderExpr_ 1 functions e2 ctx2
                      , text "]"
                      ]
                      
          Cons e0 e1 ->
              let
                  ctx0 = Monocle.compose ctx Context.cons0
                  ctx1 = Monocle.compose ctx Context.cons1
              in
                  paren (prec>0) <|
                      span []
                      [ renderExpr_ 1 functions e0 ctx0 
                      , redexSpan functions expr ctx [text ":"]
                      , renderExpr_ 1 functions e1 ctx1 
                      ]

          InfixOp op e0 e1 ->
              let
                  ctx0 = Monocle.compose ctx Context.infixOp0
                  ctx1 = Monocle.compose ctx Context.infixOp1
              in
                  paren (prec>0) <|
                      span []
                      [ renderExpr_ 1 functions e0 ctx0 
                      , redexSpan functions expr ctx [text (Pretty.formatOperator op)]
                      , renderExpr_ 1 functions e1 ctx1 
                      ]

          App e0 args ->
              let
                  n = List.length args - 1
                  ctx0 = Monocle.compose ctx Context.app0
                  ctxs = List.map (\i -> Monocle.compose ctx (Context.appArg i))
                         (List.range 0 n)
                  items = List.intersperse (text " ")
                          <| List.map2 (\ei ctxi -> renderExpr_ 1 functions ei ctxi) args ctxs
                              
              in
                  paren (prec>0) <|
                      span []
                      <| redexSpan functions expr ctx [renderExpr_ 1 functions e0 ctx0] ::
                          text " " ::  items
                  
          Lam xs e1 ->
              text (Pretty.prettyExpr_ prec expr)
                  
          IfThenElse e1 e2 e3 ->
              paren (prec>0) <|
              span []
                  [ redexSpan functions expr ctx [ text "if " ]
                  , renderExpr functions e1 (Monocle.compose ctx Context.if0)
                  , redexSpan functions expr ctx [text " then "]
                  , text (Pretty.prettyExpr e2)
                  , redexSpan functions expr ctx [text " else "]
                  , text (Pretty.prettyExpr e3) 
                  ]
                  
          Fail msg -> span [class "error"] [ text msg ]

-- render a redex with info tooltip
redexSpan : Functions -> Expr -> Context -> List (Html Msg) -> Html Msg
redexSpan functions expr ctx elements =
    case Eval.redex functions expr of
        Just (_, info) ->
            span [class "redex", onClick (Step ctx)]
                <| span [class "info"] [text info] :: elements
        Nothing ->
            span [] elements
        
                      
paren : Bool -> Html msg -> Html msg
paren b html
    =  if b then
           span [] [text "(", html, text ")"]
       else
           html
        

