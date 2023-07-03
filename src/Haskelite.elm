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
import Dict exposing (Dict)
import Browser


type Model
    = Editing EditModel            -- while editing
    | Reducing ReduceModel         -- while doing evaluations
    | Panic String                 -- when initialization failed

type alias EditModel
    = { inputExpr : String                   -- input expression
      , inputDecls : String                  -- function declarations
      , parseResult : Result String Program  -- result of parsing
      , prelude : List Bind
      }

type alias ReduceModel
    = { current : Machine.Conf           -- current configuration
      , next : Maybe Machine.Conf        -- optional next configuration
      , previous : List Machine.Conf     -- list of previous configs
      , inputExpr : String               -- saved input text (to go back to editing)
      , inputDecls : String
      }

    
type Msg
    = Previous           -- undo one evaluation step
    | Next               -- next outermost step
    | Reset              -- reset evaluation
    | ModifyExpr String   -- parse expression
    | ModifyDecls String  -- parse declarations
    | Edit               -- go into editing mode
    | Evaluate           -- go into reduction mode


-- the main entry point for the app
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

        
-- initializing the application
init : {expression:String, declarations:String} -> (Model, Cmd msg)
init config  = (initModel config, Cmd.none)

initModel : {expression:String, declarations:String} -> Model
initModel config
    = case Prelude.preludeResult of
          Err msg ->
              Panic msg
          Ok prelude ->
              let
                  result = parseAndTypecheck prelude config.expression config.declarations
              in
                  Editing
                  { inputExpr = config.expression
                  , inputDecls = config.declarations
                  , parseResult = result
                  , prelude = prelude
                  }

-- the first argument are the prelude bindings                  
parseAndTypecheck : List Bind -> String -> String -> Result String Program
parseAndTypecheck prelude expression declarations
    = HsParser.parseProgram expression declarations |>
      Result.andThen (\prog -> Typecheck.tcProgram prelude prog |>
      Result.andThen (\_ -> Ok prog))

        
view : Model -> Html Msg
view model =
    case model of
        Editing m -> editingView m
        Reducing m -> reduceView m
        Panic msg -> panicView msg

panicView : String -> Html msg
panicView msg =
    span [class "error"] [text msg]
                     
editingView : EditModel -> Html Msg
editingView model =
    div [] [ span [] [
                  input [ placeholder "Enter an expression"
                        , value model.inputExpr
                        , size 65
                        , spellcheck False
                        , class "editline"
                        , onInput ModifyExpr
                        ]  []
                 , button [ class "navbar", onClick Evaluate ] [ text "Evaluate" ]
               ]
           , br [] []
           , textarea [ placeholder "Enter function definitions"
                      , value model.inputDecls
                      , rows 24
                      , cols 80
                      , spellcheck False
                      , onInput ModifyDecls
                      ] []
           , br [] []
           , case model.parseResult of
                 Err msg -> if model.inputDecls == "" &&
                               model.inputExpr == "" then
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
        Edit ->
            initModel {expression=model.inputExpr, declarations=model.inputDecls}
            
        _ ->
            Reducing model
                 



                
           
                
editUpdate : Msg -> EditModel -> Model 
editUpdate msg model =
    case msg of
        ModifyExpr string ->
            let result = HsParser.parseProgram string model.inputDecls
            in
                Editing { model | inputExpr=string, parseResult=result }

        ModifyDecls string ->
            let result = HsParser.parseProgram model.inputExpr string 
            in
                Editing { model | inputDecls=string, parseResult=result }
        
        Evaluate ->
            case parseAndTypecheck model.prelude model.inputExpr model.inputDecls of
                Ok (Letrec binds expr) ->
                    let
                        heap0 = Heap.fromBinds (model.prelude ++ binds)
                        conf0 = (heap0, Machine.E expr, [])
                    in Reducing
                        { current = conf0
                        , next = Machine.next conf0
                        , previous = []
                        , inputExpr = model.inputExpr
                        , inputDecls = model.inputDecls
                        }
                Err msg1 ->
                    Editing {model | parseResult = Err msg1}
        _ ->
            Editing model
                       
subscriptions : Model -> Sub msg
subscriptions _ = Sub.none


-- render a configuration to HTML
renderConf : Machine.Conf -> Html msg
renderConf conf
    = case Machine.prettyConf conf of
          Just (txt,Just info) ->
              div [class "line"]
                  [ text txt
                  , div [class "info"] [ text info]
                  ]
          Just (txt,Nothing) ->
              div [class "line"]
                  [ text txt ]                  
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
