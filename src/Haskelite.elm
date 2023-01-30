{-
  Haskelite, a single-step interpreter for a subset of Haskell.
  Main module exporting an interactive HTML entity
  Pedro Vasconcelos, 2021
-}
module Haskelite exposing (..)

import AST exposing (Expr(..), Program(..), Info, Name)
import HsParser
import Eval exposing (EvalEnv)
import Typecheck
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


type Model
    = Editing EditModel
    | Reducing ReduceModel

type alias EditModel
    = { inputExpr : String                   -- input expression
      , inputDecls : String                  -- function declarations
      , parseResult : Result String Program      -- parsing results
      }

type alias ReduceModel
    = { expression : Expr                -- current expression
      , next : Maybe (Expr, Info)        -- next reduction
      , previous : List (Expr, Info)     -- list of previous steps
      , evalEnv : EvalEnv                -- evaluation environment (include Prelude)
      , inputExpr : String               -- inputs (to go back to editing)
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

        
-- * inicializing the application
init : {expression:String, declarations:String} -> (Model, Cmd msg)
init config  = (initModel config, Cmd.none)

initModel : {expression:String, declarations:String} -> Model
initModel config
    = let result = parseAndTypecheck config.expression config.declarations
      in
          Editing
          { inputExpr = config.expression
          , inputDecls = config.declarations
          , parseResult = result
          }

        
parseAndTypecheck : String -> String -> Result String Program
parseAndTypecheck expression declarations
    = HsParser.parseProg expression declarations |>
      Result.andThen Typecheck.tcProgram 

        
view : Model -> Html Msg
view model =
    case model of
        Editing m -> editingView m
        Reducing m -> reduceView m

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
               {-
           , case model.resultExpr of
                 Err msg -> if model.inputExpr /= "" then
                                span [class "error"] [text msg]
                            else span [] []
                 _ -> span [] []
           , br [] []
                -}
           , textarea [ placeholder "Enter function definitions"
                      , value model.inputDecls
                      , rows 24
                      , cols 80
                      , spellcheck False
                      , onInput ModifyDecls
                      ] []
           , br [] []
           , case model.parseResult of
                 Err msg -> if model.inputDecls == "" && model.inputExpr == "" then
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
    case model of
        Reducing submodel ->
            (reduceUpdate msg submodel, Cmd.none)
        Editing submodel ->
            (editUpdate msg submodel, Cmd.none)

        
reduceUpdate : Msg -> ReduceModel -> Model
reduceUpdate msg model =
    case msg of
        Previous ->
            case model.previous of
                ((oldExpr, _) :: rest) ->
                    Reducing
                    { model
                        | expression = oldExpr
                        , next = Eval.reduceNext model.evalEnv oldExpr
                        , previous = rest
                    }
                [] ->
                    Reducing model

        Next ->
            case model.next of
                Just (newExpr, info) ->
                    Reducing
                    { model
                        | expression = newExpr
                        , next = Eval.reduceNext model.evalEnv newExpr
                        , previous = (model.expression, info) :: model.previous
                    }
                Nothing ->
                    Reducing model
                        
        Reset ->
            case List.last model.previous of
                Just (expr,_) ->
                    Reducing
                    { model
                        | expression = expr
                        , next = Eval.reduceNext model.evalEnv expr 
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
            let result = HsParser.parseProg string model.inputDecls
            in
                Editing { model | inputExpr=string, parseResult=result }

        ModifyDecls string ->
            let result = HsParser.parseProg model.inputExpr string 
            in
                Editing { model | inputDecls=string, parseResult=result }
        
        Evaluate ->
            case parseAndTypecheck model.inputExpr model.inputDecls of
                Ok (Letrec binds expr) ->
                    let env = Eval.toEvalEnv (Prelude.bindings ++ binds)
                    in Reducing
                        { expression = expr
                        , next = Eval.reduceNext env expr
                        , previous = []
                        , evalEnv = env
                        , inputExpr = model.inputExpr
                        , inputDecls = model.inputDecls
                        }
                Err msg1 ->
                    Editing {model | parseResult = Err msg1}
        _ ->
            Editing model
                       
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


                      
                      
paren : Bool -> Html msg -> Html msg
paren b html
    =  if b then
           span [] [text "(", html, text ")"]
       else
           html
        

