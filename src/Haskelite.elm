{-
  Haskelite, a single-step interpreter for a subset of Haskell.
  Main module exporting an interactive HTML entity
  Pedro Vasconcelos, 2021-23
-}
module Haskelite exposing (..)

import AST exposing (Expr(..), Program(..), Bind, Info, Name)
import HsParser
import Machine
import Heap
import Typecheck
import Parser
import Pretty
import Prelude
-- import Context exposing (Context)
import Monocle.Optional as Monocle
import Html exposing (..)
import Html.Attributes exposing (value, class, placeholder, disabled,
                                     size, rows, cols, spellcheck)
import Html.Events exposing (on, onClick, onInput)
import Platform.Cmd as Cmd
import Platform.Sub as Sub

import List.Extra as List
import Browser
import Debug

type alias Inputs 
    = { expression:String, declarations:String }

type Model
    = Editing EditModel            -- while editing
    | Reducing ReduceModel         -- while doing evaluations
    | Panic String                 -- when initialization failed

type alias EditModel
    = { inputs : Inputs                 -- user inputs
      , parsed : Result String Program  -- result of parsing
      , prelude : List Bind             -- prelude bindings
      }

type alias ReduceModel
    = { current : Machine.Conf           -- current configuration
      , previous : List Machine.Conf     -- list of previous configs
      , next : Maybe Machine.Conf        -- optional next configuration
      , inputs : Inputs                  -- saved inputs (to go back to editing)
      }

    
type Msg
    = Previous           -- undo one evaluation step
    | Next               -- next outermost step
    | Reset              -- reset evaluation
    | EditMode           -- go into editing mode
    | EvalMode           -- go into evaluation mode
    | Edit Inputs        -- modify inputs

      

-- the main entry point for the app
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

        
-- initializing the application
init : Inputs -> (Model, Cmd msg)
init inputs  = (initModel inputs, Cmd.none)

initModel : Inputs -> Model
initModel inputs
    = case Prelude.preludeResult of
          Err msg ->
              Panic msg
          Ok prelude ->
              let
                  result = parseAndTypecheck prelude inputs
              in
                  Editing
                  { inputs = inputs
                  , parsed = result
                  , prelude = prelude
                  }

-- parse and typecheck input expression and declararations
-- the first argument are the bindings for the prelude
parseAndTypecheck : List Bind -> Inputs -> Result String Program
parseAndTypecheck prelude inputs
    = HsParser.parseProgram inputs.expression inputs.declarations |>
      Result.andThen (\prog -> Typecheck.tcProgram prelude prog |>
      Result.andThen (\_ -> Ok prog))

        
view : Model -> Html Msg
view model =
    case model of
        Editing m ->
            editingView m
        Reducing m ->
            reduceView m
        Panic msg ->
            panicView msg

panicView : String -> Html msg
panicView msg =
    span [class "error"] [text msg]
                     
editingView : EditModel -> Html Msg
editingView model =
    div [] [ span [] [
                  input [ placeholder "Enter an expression"
                        , value model.inputs.expression
                        , size 65
                        , spellcheck False
                        , class "editline"
                        , onInput (\str -> Edit {expression=str,declarations=model.inputs.declarations})
                        ]  []
                 , button
                      [ class "navbar", onClick EvalMode ]
                      [ text "Evaluate" ]
               ]
           , br [] []
           , textarea [ placeholder "Enter function definitions"
                      , value model.inputs.declarations
                      , rows 24
                      , cols 80
                      , spellcheck False
                      , onInput (\str -> Edit {expression=model.inputs.expression,declarations=str})
                      ] []
           , br [] []
           , case model.parsed of
                 Err msg -> if model.inputs.expression == "" &&
                               model.inputs.declarations == "" then
                                span [] []
                             else
                                span [class "error"] [text msg]
                 _ -> span [] []
           ]

reduceView : ReduceModel -> Html Msg
reduceView model =
    div []
        [ span [] [ button [ class "navbar"
                           , disabled (not (List.isEmpty model.previous))
                           , onClick EditMode] [text "Edit"]
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
                          [ renderConf model.current ]
                 ]
        ]

         
lineView : Machine.Conf -> Html Msg
lineView conf =
    renderConf conf 

{-
        , div [class "info"] [ text (Pretty.prettyInfo info) ]              
        ]
-}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case model of
        Reducing submodel ->
            (reduceUpdate msg submodel, Cmd.none)
        Editing submodel ->
            (editUpdate msg submodel, Cmd.none)
        Panic _ ->
            (model, Cmd.none)

        
reduceUpdate : Msg -> ReduceModel -> Model
reduceUpdate msg model =
    case msg of
        Previous ->
            case model.previous of
                (first :: rest) ->
                    Reducing
                    { model
                        | current = first
                        , next = Just model.current
                        , previous = rest
                    }
                [] ->
                    Reducing model

        Next ->
            case model.next of
                Just new ->
                    Reducing
                    { model
                        | current = new
                        , next = Machine.next new
                        , previous = model.current :: model.previous
                    }
                Nothing ->
                    Reducing model
                        
        Reset ->
            case List.last model.previous of
                Just first ->
                    Reducing
                    { model
                        | current = first
                        , next = Machine.next first 
                        , previous = []
                    }
                Nothing ->
                    Reducing model
        EditMode ->
            initModel model.inputs
            
        _ ->
            Reducing model
                 



                
           
                
editUpdate : Msg -> EditModel -> Model 
editUpdate msg model =
    case msg of
        Edit inputs ->
            let result = HsParser.parseProgram inputs.expression inputs.declarations
            in
                Editing { model | inputs=inputs, parsed=result }
        
        EvalMode ->
            case parseAndTypecheck model.prelude model.inputs of
                Ok (Letrec binds expr) ->
                    let
                        heap0 = Heap.fromBinds (model.prelude ++ binds)
                        conf0 = Machine.start heap0 expr
                    in Reducing
                        { current = conf0
                        , next = Machine.next conf0
                        , previous = []
                        , inputs = model.inputs
                        }
                Err msg1 ->
                    Editing {model | parsed = Err msg1}
        _ ->
            Editing model
                       
subscriptions : Model -> Sub msg
subscriptions _ = Sub.none


-- render a configuration to HTML
renderConf : Machine.Conf -> Html msg
{-             
renderConf conf = div [class "line"]
                  [ text <| Debug.toString
                        (Machine.getControl conf,
                         Machine.getStack conf) ]
-}
renderConf conf
    = case Machine.prettyConf conf of
          Just txt ->
              div [class "line"]
                  [ text txt
                  , case Machine.justifies conf of
                        Just info ->
                            div [class "info"] [text info]
                        Nothing ->
                            span [] []
                  ]
          Nothing ->
              span [] []


{-                     
-- render an expression to HTML; toplevel function
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


                      
                      
paren : Bool -> Html msg -> Html msg
paren b html
    =  if b then
           span [] [text "(", html, text ")"]
       else
           html
        
-}
