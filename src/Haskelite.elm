{-
  Haskelite, a single-step interpreter for a subset of Haskell.
  Main module exporting the interactive HTML element
  Pedro Vasconcelos, 2021-23
-}
module Haskelite exposing (..)

import AST exposing (Expr(..), Program(..), Module, Bind, Info, Name)
import HsParser
import Machine
import Machine.Types as Machine
import Machine.Heap as Heap
import Typecheck exposing (TyEnv, KindEnv)
import Parser
import Pretty
import Prelude

import Dict
import Set exposing (Set)

import Html exposing (..)
import Html.Attributes exposing (type_, class, value, style, placeholder, checked,
                                     disabled, size, rows, cols, spellcheck)
import Html.Events exposing (on, onClick, onInput)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Browser
import CustomElement.CodeEditor as Editor

import List.Extra as List

-- startup flags
type alias Flags
    = { expression:String, declarations:String }

type Model
    = Editing EditModel            -- while editing
    | Reducing ReduceModel         -- while doing evaluations
    | Panic String                 -- when initialization failed

type alias EditModel
    = { flags : Flags                    -- user inputs
      , parsed : Result String Program   -- result of parsing and typechecking
      , prelude : List Bind              -- prelude definitions
      , kindEnv : KindEnv                -- kind and type environments
      , typeEnv : TyEnv                  -- for primitives and prelude
      }

-- a labelled transition step: the machine configuration
-- plus a justification the last transition
type alias Step
    = (Machine.Conf, Info)   
           
type alias ReduceModel
    = { current : Step             -- current configuration
      , previous : List Step       -- list of previous steps
      , next : Maybe Step          -- optional next step
      , flags : Flags              -- saved flags (to go back to editing)
      , options : Pretty.Options   -- displaying options
      , skipped : Set Name        -- set of function names to skip
      }

   
    
type Msg
    = Previous           -- undo one evaluation step
    | Next               -- next outermost step
    | Reset              -- reset evaluation
    | EditMode           -- go into editing mode
    | EvalMode           -- go into evaluation mode
    | Edit Flags         -- modify flags
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
init : Flags -> (Model, Cmd msg)
init flags = (initModel flags, Cmd.none)

initModel : Flags -> Model
initModel flags
    = case Prelude.prelude of
          Err msg ->
              Panic msg
          Ok (binds, kenv, tenv) ->  
              let
                  result = parseAndTypecheck kenv tenv flags
              in
                  Editing
                  { flags = flags
                  , parsed = result
                  , prelude = binds
                  , kindEnv =  kenv
                  , typeEnv = tenv
                  }

-- parse and typecheck use expression and function declararations
-- the first argument is the prelude
parseAndTypecheck : KindEnv -> TyEnv -> Flags -> Result String Program
parseAndTypecheck kenv tenv flags
    = HsParser.parseProgram flags.expression flags.declarations |>
      Result.andThen (\prog -> Typecheck.tcMain kenv tenv prog |>
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
    let oldflags = model.flags
    in 
    div [] [ Editor.codeEditor
                 [ Editor.editorValue model.flags.declarations
                 , Editor.onEditorChanged (\str -> Edit {oldflags|declarations=str}) ]
                 []
           , br [] []
           , span [] [ span [class "editline"] [text ">>> "]
                     , input [ placeholder "Enter an expression"
                             , value model.flags.expression
                             , size 65
                             , spellcheck False
                             , class "editline"
                             , onInput (\str -> Edit {oldflags | expression=str})
                             ]  []
                     , button
                           [ class "navbar", onClick EvalMode ]
                           [ text "Evaluate" ]
                     ]
           , br [] []
           , case model.parsed of
                 Err msg -> if model.flags.expression == "" &&
                               model.flags.declarations == "" then
                                span [] []
                             else
                                span [class "error"] [text msg]
                 _ -> span [] []
           ]

reduceView : ReduceModel -> Html Msg
reduceView model =
    let linecount = List.length model.previous
    in 
    div []
        [ -- fill the lines div in reverse order;
        -- the CSS enabled a custom flow direction to ensure the
        -- current line always visible when scrolling is needed
         div [class "lines"]
             <|  [ div [class "current"]
                       [ renderStep model.options linecount linecount model.current ]
                 ]
                 ++
                 List.map2 (renderStep model.options linecount)
                       (List.reverse <| List.range 0 (linecount-1)) model.previous
        , div [] [ span [] [ button [ class "navbar"
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
                 , span [style "padding-left" "20px"] []
                 , checkbox model.options.prettyLists
                            (Toggle toggleLists) "Pretty-print lists"
                 , checkbox model.options.prettyEnums
                           (Toggle toggleEnums) "Pretty-print enumerations"
                 ]
        ]

{-        
skippedNames : List Name -> Html a
skippedNames names
    = case names of
          [] ->
              span [] []
          _ ->
              span [] [ text "Skipping: ",
                          code [] <|
                           List.intersperse (text " ") (List.map text names) ]
-}          
        

checkbox : Bool -> msg -> String -> Html msg
checkbox b msg name =
    label
        [class "navbar"] 
        [ input [ type_ "checkbox", onClick msg, checked b ] []
        , text name
        ]
        
toggleLists : Pretty.Options -> Pretty.Options
toggleLists opts = { opts | prettyLists = not (opts.prettyLists) }

toggleEnums : Pretty.Options -> Pretty.Options
toggleEnums opts = { opts | prettyEnums = not (opts.prettyEnums) }
                

renderStep  : Pretty.Options -> Int -> Int -> Step -> Html Msg
renderStep opts largest number (conf, info)
    = case Pretty.prettyConf opts conf of
          Just txt ->
              div [class "line"]
                  [ span [class "linenumber"]
                        [text (rightAlign largest number ++ ". ")]
                  , text txt
                  , div [class "info"] [text info]
                  ]
          Nothing ->
              span [] []

-- right align a number; first argument is the largest number in the sequence
-- use a Unicode non breakable space to prevent HTML from eating up the formating
rightAlign : Int -> Int -> String
rightAlign largest number
    = String.padLeft (decimalDigits largest) '\u{00a0}' (String.fromInt number)

-- get the number of decimal digits          
decimalDigits : Int -> Int
decimalDigits n = ceiling (logBase 10.0 (max 1 (toFloat n)+1))
                  

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
                        , next = Machine.next model.skipped (Tuple.first new)
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
                        , next = Machine.next Set.empty (Tuple.first start)
                        , previous = []
                    }
                Nothing ->
                    Reducing model
        EditMode ->
            initModel model.flags
            
        Toggle f ->
            Reducing {model | options = f model.options}

        _ ->
            Reducing model
                 
                
editUpdate : Msg -> EditModel -> Model 
editUpdate msg model =
    case msg of
        Edit flags ->
            let result = HsParser.parseProgram flags.expression flags.declarations
            in
                Editing { model | flags=flags, parsed=result }
        
        EvalMode ->
            case parseAndTypecheck model.kindEnv model.typeEnv model.flags of
                Ok (LetProg mod expr) ->
                    let
                        heap0 = Heap.fromBinds (model.prelude ++ mod.binds)
                        conf0 = Machine.start heap0 expr
                        skipped = Set.fromList mod.skip
                    in Reducing
                        { current = (conf0, "initial expression")
                        , next = Machine.next skipped conf0
                        , previous = []
                        , flags = model.flags
                        , options = Pretty.defaultOpts
                        , skipped = skipped
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
