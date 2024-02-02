{-
  Haskelite, a single-step interpreter for a subset of Haskell.
  Main module exporting the interactive HTML element
  Pedro Vasconcelos, 2021-24
-}
module Haskelite exposing (main)

import AST exposing (Expr(..), Program(..), Module, Bind, Info, Name)
import HsParser
import Machine
import Machine.Types as Machine
import Machine.Heap as Heap
import Typecheck exposing (TyEnv, KindEnv)
import Parser
import PrettyPrinter 
import Prelude

import Dict
import Set exposing (Set)
import List.Extra as List

import Pretty exposing (Doc)
import Pretty.Renderer as Pretty

import Html exposing (..)
import Html.Attributes exposing (type_, class, value, style, placeholder, checked,
                                 disabled, size, rows, cols, spellcheck, tabindex)
import Html.Events exposing (on, onClick, onInput)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Browser
import CustomElement.CodeEditor as Editor
import Keyboard.Event exposing (KeyboardEvent, considerKeyboardEvent)
import Keyboard.Key exposing (Key(..))


-- startup flags
type alias Flags
    = { expression:String, declarations:String }

type Model
    = Editing EditModel            -- while editing
    | Reducing ReduceModel         -- while doing evaluations
    | Panic String                 -- when initialization failed

type alias EditModel
    = { flags : Flags                    -- user inputs
      , parsedExpr : Result String Expr     -- results of parsing
      , parsedDecls : Result String Module
      , typeChecked : Result String (Maybe Program) -- result of type checking
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
      , flags : Flags              -- saved inputs (to go back to editing)
      , options : Options          -- displaying options
      }

type alias Options
    = { prettyLists : Bool     -- should we prettify lists?
      , prettyEnums : Bool     -- should we prettify Prelude enum functions?
      , layout : Bool
      }

-- check for final evaluation step    
isFinal : ReduceModel -> Bool
isFinal model
    = Machine.checkFinal (Tuple.first model.current)

isInit : ReduceModel -> Bool
isInit model
    = List.isEmpty model.previous
      
defaultOpts : Options
defaultOpts
    = { prettyLists = True, prettyEnums = True, layout = True }

    
type Msg
    = Previous           -- undo one evaluation step
    | Next               -- next outermost step
    | Reset              -- reset evaluation
    | EditMode           -- go into editing mode
    | EvalMode           -- go into evaluation mode
    | EditExpr String    -- modify expression
    | EditDecls String   -- modify declarations
    | Toggle (Options -> Options) -- modify options


     
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
                  result1 = HsParser.parseExpr flags.expression
                  result2 = HsParser.parseModule flags.declarations
                  result3 = typecheck kenv tenv result1 result2
              in
                  Editing
                  { flags = flags
                  , parsedExpr = result1
                  , parsedDecls = result2
                  , typeChecked = result3
                  , prelude = binds
                  , kindEnv =  kenv
                  , typeEnv = tenv
                  }


-- typecheck if parsing was succesful                  
typecheck : KindEnv -> TyEnv ->
            (Result String Expr) ->
            (Result String Module) -> Result String (Maybe Program)
typecheck kenv tenv parsedExpr parsedDecls
    = case (parsedExpr, parsedDecls) of
          (Ok expr, Ok decls) ->
              Typecheck.tcMain kenv tenv (LetProg decls expr) |>
              Result.andThen (\_ -> Ok (Just (LetProg decls expr)))
          _ ->
             Ok Nothing  -- ok, nothing to check yet

          
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
                 , Editor.onEditorChanged EditDecls ]
                 []
           , case model.parsedDecls of
                 Err msg ->
                     if model.flags.declarations == "" then
                         span [] []
                     else
                         span [class "error"] [text msg]
                 _ ->
                     span [] []                             
           , br [] []
           , span [] [ span [class "editline"] [text ">>> "]
                     , input [ placeholder "Enter an expression"
                             , value model.flags.expression
                             , spellcheck False
                             , class "editline"
                             , onInput EditExpr
                             ]  []
                     , button
                           [ class "navbar", onClick EvalMode ]
                           [ text "Evaluate" ]
                     ]
           , br [] []
           , case model.parsedExpr of
                 Err msg
                     -> if model.flags.expression == ""  then
                            span [] []
                        else
                            span [class "error"] [text msg]
                 _ ->
                     span [] []
           , case model.typeChecked of
                 Err msg ->
                     span [class "error"] [text msg]
                 _ ->
                     span [] []

           ]

reduceView : ReduceModel -> Html Msg
reduceView model =
    let linecount = List.length model.previous
    in 
    div [ on "keydown" (considerKeyboardEvent handleKeyEvent)
        , tabindex 0 ]
        [ -- fill the lines div in reverse order;
        -- the CSS enabled a custom flow direction to ensure the
        -- current line always visible when scrolling is needed
         div [ class "lines" ]
             <|  [ div [class "current"]
                       [ renderStep model.options linecount linecount model.current ]
                 ]
                 ++
                 List.map2 (renderStep model.options linecount)
                       (List.reverse <| List.range 0 (linecount-1)) model.previous
        , div [] [ span [] [ button [ class "navbar"
                           , disabled (isInit model)
                           , onClick EditMode] [text "Edit"]
                  , button [ class "navbar"
                           , onClick Reset
                           ] [text "Reset"]
                  , button [ class "navbar"
                           , disabled (isInit model)
                           , onClick Previous] [text "< Prev"]
                  , button [ class "navbar"
                           , disabled (isFinal model)
                           , onClick Next] [text "Next >"]
                 , span [class "options"] [
                         label [] [text "Pretty-printing"]                         
                       ,  checkbox model.options.prettyLists
                             (Toggle toggleLists) "lists"
                       , checkbox model.options.prettyEnums
                             (Toggle toggleEnums) "enumerations"
                       , checkbox model.options.layout
                             (Toggle toggleLayout) "layout"
                            ]
                       ]
                 ]
        ]


handleKeyEvent : KeyboardEvent -> Maybe Msg
handleKeyEvent ev
    = case ev.keyCode of
          Enter ->
              Just Next
          PageDown ->
              Just Next
          Down ->
              Just Next
          Right ->
              Just Next
          Backspace ->
              Just Previous
          PageUp ->
              Just Previous
          Left ->
              Just Previous
          Up ->
              Just Previous
          Escape ->
              Just EditMode
          _ ->
              Nothing
        

checkbox : Bool -> msg -> String -> Html msg
checkbox b msg name =
    label
        [class "navbar"] 
        [ input [ type_ "checkbox", onClick msg, checked b ] []
        , text name
        ]
        
toggleLists : Options -> Options
toggleLists opts = { opts | prettyLists = not (opts.prettyLists) }

toggleEnums : Options -> Options
toggleEnums opts = { opts | prettyEnums = not (opts.prettyEnums) }

toggleLayout : Options -> Options
toggleLayout opts = { opts | layout = not (opts.layout) }
                   

-- render a single numbered reduction line                   
renderStep  : Options -> Int -> Int -> Step -> Html Msg
renderStep opts largest number (conf, info)
    = let step = {conf=conf, number=number, largest=largest}
      in case PrettyPrinter.prettyConfStep opts step of
          Just html ->
              div [class "line"]
                  [ html, div [class "info"] [text info] ]
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
                    Reducing { model | current = last, previous = rest }
                [] ->
                    Reducing model

        Next ->
            case Machine.labelledTransition (Tuple.first model.current) of
                Just new ->
                    Reducing { model | current = new
                             , previous = model.current :: model.previous
                             }
                Nothing ->
                    Reducing model

                       
        Reset ->
            case List.last model.previous of
                Just start ->
                    Reducing { model | current = start, previous = [] }
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
        EditExpr str ->
            let result = HsParser.parseExpr str
            in Editing { model |
                         flags = { expression = str
                                 , declarations = model.flags.declarations }
                       , parsedExpr = result
                       , typeChecked = Ok Nothing
                       }
        EditDecls str ->
            let result = HsParser.parseModule str
            in Editing { model |
                         flags = { expression = model.flags.expression
                                 , declarations = str }
                       , parsedDecls = result
                       , typeChecked = Ok Nothing
                       }
        
        EvalMode ->
            case (typecheck model.kindEnv model.typeEnv
                      model.parsedExpr model.parsedDecls) of
                Ok (Just (LetProg mod expr)) ->
                    let
                        heap0 = Heap.fromBinds (model.prelude ++ mod.binds)
                        conf0 = Machine.start heap0 expr
                    in Reducing
                        { current = (conf0, "initial expression")
                        -- , next = Machine.labelledTransition conf0
                        , previous = []
                        , flags = model.flags
                        , options = defaultOpts
                        }
                Ok Nothing ->
                    Editing { model | typeChecked = Ok Nothing }
                Err msg1 ->
                    Editing {model | typeChecked = Err msg1}
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
