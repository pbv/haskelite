{-
  Haskelite, a single-step interpreter for a subset of Haskell.
  Main module exporting the interactive HTML element
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

import Html exposing (..)
import Html.Attributes exposing (type_, class, value, style, placeholder, checked,
                                     disabled, size, rows, cols, spellcheck)
import Html.Events exposing (on, onClick, onInput)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Browser

import List.Extra as List


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

-- a labelled transition step: the machine configuration
-- plus a justification the last transition
type alias Step
    = (Machine.Conf, Info)   
           
type alias ReduceModel
    = { current : Step             -- current configuration
      , previous : List Step       -- list of previous steps
      , next : Maybe Step          -- optional next step
      , inputs : Inputs            -- saved inputs (to go back to editing)
      , options : Pretty.Options          -- displaying options
      }

   
    
type Msg
    = Previous           -- undo one evaluation step
    | Next               -- next outermost step
    | Reset              -- reset evaluation
    | EditMode           -- go into editing mode
    | EvalMode           -- go into evaluation mode
    | Edit Inputs        -- modify inputs
    | Toggle (Pretty.Options -> Pretty.Options) -- modify options

     
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
                  , checkbox model.options.prettyLists
                      (Toggle toggleLists) "Pretty-print lists"
                  , checkbox model.options.prettyEnums
                      (Toggle toggleEnums) "Pretty-print enums"
                  ]
        -- fill the lines div in reverse order;
        -- the CSS enabled a custom flow direction to ensure the
        -- current line always visible when scrolling is needed
        , div [class "lines"]
             <|  [ div [class "current"]
                          [ renderStep model.options  model.current ]
                 ]
                 ++
                 List.map (renderStep model.options) model.previous
        ]


checkbox : Bool -> msg -> String -> Html msg
checkbox b msg name =
    label
        [ style "padding" "20px" ]
        [ input [ type_ "checkbox", onClick msg, checked b ] []
        , text name
        ]
        
toggleLists : Pretty.Options -> Pretty.Options
toggleLists opts = { opts | prettyLists = not (opts.prettyLists) }

toggleEnums : Pretty.Options -> Pretty.Options
toggleEnums opts = { opts | prettyEnums = not (opts.prettyEnums) }
                

renderStep  : Pretty.Options -> Step -> Html Msg
renderStep opts (conf, info)
    = case Pretty.prettyConf opts conf of
          Just txt ->
              div [class "line"]
                  [ text txt,
                    div [class "info"] [text info]
                  ]
          Nothing ->
              span [] []


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
                (last :: rest) ->
                    Reducing
                    { model
                        | current = last
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
                        , next = Machine.next (Tuple.first new)
                        , previous = model.current :: model.previous
                    }
                Nothing ->
                    Reducing model
                        
        Reset ->
            case List.last model.previous of
                Just start ->
                    Reducing
                    { model
                        | current = start
                        , next = Machine.next (Tuple.first start)
                        , previous = []
                    }
                Nothing ->
                    Reducing model
        EditMode ->
            initModel model.inputs
            
        Toggle f ->
            Reducing { model | options = f model.options}

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
                Ok (LetProg binds expr) ->
                    let
                        heap0 = Heap.fromBinds (model.prelude ++ binds)
                        conf0 = Machine.start heap0 expr
                    in Reducing
                        { current = (conf0, "initial expression")
                        , next = Machine.next conf0
                        , previous = []
                        , inputs = model.inputs
                        , options =
                              { prettyLists = True
                              , prettyEnums = True
                              }
                        }
                Err msg1 ->
                    Editing {model | parsed = Err msg1}
        _ ->
            Editing model
                       
subscriptions : Model -> Sub msg
subscriptions _ = Sub.none


{-
-- extra debugging stuff                 
observe : a -> b -> b
observe x y
    = let
        _ = Debug.log ">>>" x
      in  y

-}
