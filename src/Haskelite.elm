{-
  Haskelite, a single-step interpreter for a subset of Haskell.
  Main module exporting an interactive HTML entity
  Pedro Vasconcelos, 2021
-}
module Haskelite exposing (..)

import AST exposing (Expr(..), Decl, Info, Name)
import HsParser
import Eval exposing (Binds)
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
    = { expression : Expr                 -- current expression
      , next : Maybe (Expr, Info)        -- next reduction
      , previous : List (Expr, Info) -- list of previous steps
      , bindings : Binds
      , inputExpr : String
      , inputDecls : String
      , outputExpr : Result String Expr
      , outputDecls : Result String (List Decl)
      , mode : Mode
      }

type Mode
    = Editing | Reducing 

type Msg
    = Previous        -- undo one evaluation step
    | Next            -- next outermost step
    | Reset           -- reset evaluation
    | ParseExpr String   -- parse expression
    | ParseDecls String  -- parse declarations
    | Edit              -- go into editing mode
    | Evaluate         -- go into reduction mode


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
               let bindings = Eval.collectBindings decls Prelude.functions
               in 
               ({ expression = expr
                , next = Nothing
                , previous = []
                , bindings = bindings
                , inputExpr = config.expression
                , outputExpr = outputExpr
                , inputDecls = config.declarations
                , outputDecls = outputDecls
                , mode = Editing
                }
               , Cmd.none)
           _ ->
               ({ expression = Fail "syntax"
                , next = Nothing
                , previous = []
                , bindings = Prelude.functions
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
    div [] [ span [] [
                  input [ placeholder "Enter an expression"
                        , value model.inputExpr
                        , size 65
                        , spellcheck False
                        , class "editline"
                        , onInput ParseExpr
                        ]  []
                 , button [ class "navbar", onClick Evaluate ] [ text "Evaluate" ]
               ]
           , br [] []
           , case model.outputExpr of
                 Err msg -> if model.inputExpr /= "" then
                                span [class "error"] [text msg]
                            else span [] []
                 _ -> span [] []
           , br [] []
           , textarea [ placeholder "Enter function definitions"
                      , value model.inputDecls
                      , rows 24
                      , cols 80
                      , spellcheck False
                      , onInput ParseDecls
                      ] []
           , br [] []
           , case model.outputDecls of
                 Err msg -> if model.inputDecls /= "" then
                                span [class "error"] [text msg]
                            else
                                span [] []
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
                           , disabled (model.next == Nothing)
                           , onClick Next] [text "Next >"]
                  ]
        , div [class "lines"]
             <| List.map lineView (List.reverse model.previous) ++
                 [ div [class "current"]
                          [ renderExpr model.expression ]
                 ]
        ]

         
lineView : (Expr, Info) -> Html Msg
lineView (expr, info) =
    div [class "line"]
        [ renderExpr expr
        , div [class "info"] [ text (Pretty.prettyInfo info) ]
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
        Previous ->
            case model.previous of
                ((oldExpr, info) :: rest) ->
                    { model
                        | expression = oldExpr
                        , next = Eval.reduceNext model.bindings oldExpr
                        , previous = rest
                    }
                [] ->
                    model

        Next ->
            case model.next of
                Just (newExpr, info) ->
                    { model
                        | expression = newExpr
                        , next = Eval.reduceNext model.bindings newExpr
                        , previous = (model.expression, info) :: model.previous
                    }
                Nothing ->
                    model
                        
        Reset ->
            case List.last model.previous of
                Just (expr,_) ->
                    { model
                        | expression = expr
                        , next = Eval.reduceNext model.bindings expr 
                        , previous = [] }
                Nothing ->
                    model
        Edit ->
            { model | mode = Editing }
            
        _ ->
            model
                 

           
                
editUpdate : Msg -> Model -> Model
editUpdate msg model =
    case msg of
        ParseExpr string ->
            let
                output = Result.mapError Pretty.deadEndsToString
                         <| Parser.run HsParser.topExprEnd string
            in
                { model | inputExpr = string, outputExpr = output }

        ParseDecls string ->
            let
                output = Result.mapError Pretty.deadEndsToString
                         <| Parser.run HsParser.declListEnd string
            in
                { model | inputDecls = string, outputDecls = output }
        
        Evaluate ->
            case (model.outputExpr, model.outputDecls) of
                (Ok expr, Ok decls) ->
                    let bindings = Eval.collectBindings decls Prelude.functions
                    in 
                    { model | expression = expr
                    , next = Eval.reduceNext bindings expr
                    , previous = []
                    , bindings = bindings 
                    , mode = Reducing
                    }
                _ ->
                    model 
        _ ->
            model
                       
subscriptions : Model -> Sub msg
subscriptions _ = Sub.none

                 
-- render an expression; toplevel function
renderExpr : Expr -> Html msg
renderExpr expr =
    renderExpr_ 0 expr 


-- worker function 
renderExpr_ : Int ->  Expr -> Html msg
renderExpr_ prec expr 
    = case expr of
          Var x ->
              text <| if Pretty.isOperator x then "("++x++")" else x 
                  
          Number n ->
              paren (prec>0 && n<0) <| text (String.fromInt n)

          Boolean b ->
              text <| if b then "True" else "False"

          TupleLit args ->
              let
                  items = List.intersperse (text ",")
                          <| List.map renderExpr args 
              in
                  span [] <| (text "(" :: items) ++ [text ")"]
                  
                              
          ListLit args ->
              let
                  items = List.intersperse (text ",")
                          <| List.map renderExpr args 
              in
                  span [] <| (text "[" :: items) ++ [text "]"]

          App (Var "enumFrom") [e0] ->
              span []
                  [ text "[", renderExpr_ 1 e0, text "..]" ]

          App (Var "enumFromThen") [e0, e1] ->
              span []
                  [ text "["
                  , renderExpr_ 1 e0 
                  , text ","
                  , renderExpr_ 1 e1 
                  , text "..]"
                  ]
                      
          App (Var "enumFromTo") [e0, e1] ->
              span []
                  [ text "["
                  , renderExpr_ 1 e0 
                  , text ".."
                  , renderExpr_ 1 e1 
                  , text "]"
                  ]
              
          App (Var "enumFromThenTo") [e0, e1, e2] ->
              span []
                  [ text "["
                  , renderExpr_ 1 e0 
                  , text ","
                  , renderExpr_ 1 e1 
                  , text ".."
                  , renderExpr_ 1  e2 
                  , text "]"
                  ]
                      
          App (Var ":") [e0, e1] ->
              paren (prec>0) <|
                  span []
                      [ renderExpr_ 1  e0 
                      , text ":"
                      , renderExpr_ 1  e1 
                      ]

          InfixOp op e0 e1 ->
              paren (prec>0) <|
                  span []
                      [ renderExpr_ 1 e0 
                      , text (Pretty.formatOperator op)
                      , renderExpr_ 1 e1  
                      ]

          App e0 args ->
              let
                  items = List.intersperse (text " ")
                          <| List.map (renderExpr_ 1) args 
                              
              in
                  paren (prec>0) <|
                      span []
                      <| renderExpr_ 1 e0 :: text " " ::  items
                  
          Lam xs e1 ->
              text (Pretty.prettyExpr_ prec expr)
                  
          IfThenElse e1 e2 e3 ->
              paren (prec>0) <|
              span []
                  [ text "if " 
                  , renderExpr e1 
                  , text " then " 
                  , text (Pretty.prettyExpr e2)
                  , text " else " 
                  , text (Pretty.prettyExpr e3) 
                  ]
                
          Fail msg -> span [class "error"] [ text msg ]

          Eval e1 ->
              span [class "eval"] [ renderExpr_ prec e1 ]                      


                      
{-
-- render a redex with info tooltip
redexSpan : Binds -> Expr -> Context -> List (Html Msg) -> Html Msg
redexSpan bindings expr ctx elements =
    case Eval.redex bindings expr of
        Just (_, info) ->
            span [class "redex", onClick (Step ctx)]
                <| span [class "tooltip"] [text (Pretty.prettyInfo info)] :: elements
        Nothing ->
            span [] elements
-}        
                      
paren : Bool -> Html msg -> Html msg
paren b html
    =  if b then
           span [] [text "(", html, text ")"]
       else
           html
        

