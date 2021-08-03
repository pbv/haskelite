
module View exposing (..)

import AST exposing (Expr(..), Decl, Name)
import Haskell
import Eval exposing (Functions)
import Parser
import Pretty
import Prelude exposing (prelude)
import Context exposing (Context)
import Monocle.Optional as Monocle
import Html exposing (..)
import Html.Attributes exposing (value, class, placeholder, size)
import Html.Events exposing (on, onClick, onInput, onSubmit, keyCode)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Json.Decode as Json

import List.Extra as List
import Dict exposing (Dict)
import Browser


type alias ReduceModel
    = { expression : Expr              -- current expression
      , previous : List (Expr, String) -- list of previous steps
      }

type alias EditModel
    = { input : String              -- input string
      , output : Result String Expr  -- result of parsing
      }



type Model
    = Reduce ReduceModel
    | Editing EditModel

type Msg
    = Eval Context 
    | Reset
    | Edit String
    | KeyDown Int


-- global declarations from the prelude      
decls : List Decl
decls =
    case Parser.run Haskell.declList prelude of
        Ok l -> l
        Err _ -> []

functions : Functions                 
functions = Eval.collectFunctions decls Eval.primitives

init : String -> (Model, Cmd msg)      
init str =
    let submodel = editInit str
    in case submodel.output of
           Ok expr ->
               (Reduce (reduceInit expr), Cmd.none)
           Err _ ->
               (Editing submodel, Cmd.none)

                 

editInit : String -> EditModel
editInit str =
    let
        result = Result.mapError Pretty.deadEndsToString
                 <| Parser.run Haskell.topExprEnd str
    in { input = str
       , output = result
       }

reduceInit : Expr -> ReduceModel
reduceInit expr = 
    { expression = expr
    , previous = []
    }
        
view : Model -> Html Msg
view model =
    case model of
        Editing submodel -> editingView submodel
        Reduce submodel -> reduceView submodel

editingView : EditModel -> Html Msg
editingView model =
    div [] [ input [ placeholder "Enter an expression"
                   , value model.input
                   , size 80
                   , onInput Edit
                   , onKeyDown KeyDown 
                   ]  []
           , br [] []
           , case model.output of
                       Err msg -> span [class "error"] [text msg]
                       _ -> span [] []
           ]

reduceView : ReduceModel -> Html Msg
reduceView model =
    div []
        [ div [class "lines"]
             <| (List.map lineView (List.reverse model.previous) ++
                     [
                      div [class "current"]
                          [ renderExpr model.expression Context.hole ]
                     ]
                )
        , span [] [ case model.previous of
                        [] -> let str = Pretty.prettyExpr model.expression
                              in button [ onClick (Edit str) ] [text "Edit"]
                        _ -> button [ onClick Reset ] [text "Reset"]
                  ]
        ]
      
lineView : (Expr, String) -> Html Msg
lineView (expr, info) =
    div [class "line"] [
         text (Pretty.prettyExpr expr)
        , span [class "info"] [ text info ]
        ]
        


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case model of
        Reduce submodel ->
            (reduceUpdate msg submodel, Cmd.none)
        Editing submodel ->
            (editUpdate msg submodel, Cmd.none)

        
reduceUpdate : Msg -> ReduceModel -> Model
reduceUpdate msg model =
    case msg of
        Eval ctx ->
            case Eval.redexCtx functions model.expression ctx of
                Just (newExpr, info) ->
                    Reduce
                    { expression = newExpr
                    , previous = (model.expression, info) :: model.previous
                    }
                
                Nothing -> Reduce model

        Reset ->
            case List.last model.previous of
                Just (expr,_) ->
                    Reduce { expression = expr, previous = [] }
                Nothing ->
                    Reduce model
        Edit string ->
            Editing (editInit string)
            
        _ ->
            Reduce model
                
editUpdate : Msg -> EditModel -> Model
editUpdate msg model =
    case msg of
        Edit string ->
            Editing (editInit string) 
        KeyDown code ->
            if code == 13 then 
                case model.output of
                    Ok expr -> Reduce (reduceInit expr) 
                    Err _ -> Editing model
            else
                Editing model
        _ ->
            Editing model

                       
subscriptions : Model -> Sub msg
subscriptions _ = Sub.none


onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode)
      
                    
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

                                    

 -- worker function 
renderExpr : Expr -> Context -> Html Msg
renderExpr expr ctx 
    = case expr of
          Var x ->
              text <| if Pretty.isOperator x then "("++x++")" else x
                  
          Number n ->
              text (String.fromInt n)

          Boolean b ->
              text <| if b then "True" else "False"
          
          ListLit args ->
              let
                  n = List.length args - 1
                  ctxs = List.map (\i -> Monocle.compose ctx (Context.listItem i))
                          (List.range 0 n)
                  items = List.intersperse (text ", ")
                          <| List.map2 renderExpr args ctxs
              in
                  span [] <| (text "[" :: items) ++ [text "]"]

          Cons e0 e1 ->
              let
                  ctx0 = Monocle.compose ctx Context.cons0
                  ctx1 = Monocle.compose ctx Context.cons1
              in
                  paren <|
                      span []
                      [ renderExpr e0 ctx0 
                      , redexSpan expr ctx [text ":"]
                      , renderExpr e1 ctx1 
                      ]

          InfixOp op e0 e1 ->
              let
                  ctx0 = Monocle.compose ctx Context.infixOp0
                  ctx1 = Monocle.compose ctx Context.infixOp1
              in
                  paren <|
                      span []
                      [ renderExpr e0 ctx0 
                      , redexSpan expr ctx [text op]
                      , renderExpr e1 ctx1 
                      ]

          App e0 args ->
              let
                  n = List.length args - 1
                  ctx0 = Monocle.compose ctx Context.app0
                  ctxs = List.map (\i -> Monocle.compose ctx (Context.appArg i))
                         (List.range 0 n)
                  items = List.intersperse (text " ")
                          <| List.map2 (\ei ctxi -> renderExpr ei ctxi) args ctxs
                              
              in
                  paren <|
                      span []
                      <| redexSpan expr ctx [renderExpr e0 ctx0] ::
                          text " " ::  items
                  
          Lam xs e1 ->
              text (Pretty.prettyExpr expr)
                  
          IfThenElse e1 e2 e3 ->
              paren <|
              span []
                  [ redexSpan expr ctx [ text "if " ]
                  , renderExpr e1 (Monocle.compose ctx Context.if0)
                  , redexSpan expr ctx [text " then "]
                  , text (Pretty.prettyExpr e2)
                  , redexSpan expr ctx [text " else "]
                  , text (Pretty.prettyExpr e3) 
                  ]
                  
          Fail msg -> span [class "error"] [ text "!", text msg ]


redexSpan : Expr -> Context -> List (Html Msg) -> Html Msg
redexSpan expr ctx elements =
    case Eval.redex functions expr of
        Just (_, info) ->
            span [class "redex", onClick (Eval ctx)]
                <| span [class "info"] [text info] :: elements
        Nothing ->
            span [] elements
        
                      
paren : Html msg -> Html msg
paren html
    =  span [] [text "(", html, text ")"]

        

