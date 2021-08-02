
module View exposing (..)

import AST exposing (Expr(..), Name)
import Haskell
import Eval
import Parser
import Prelude exposing (prelude)
import Context exposing (Context)
import Monocle.Optional as Monocle
import Html exposing (..)
import Html.Attributes exposing (placeholder,value,style,class)
import Html.Events.Extra.Mouse as Mouse
import Platform.Cmd as Cmd
import Platform.Sub as Sub

import Dict exposing (Dict)
import Browser
import Debug


type alias Model
    = { expression : Expr
      , clientPos : (Float,Float)
      }

type Msg
    = Eval Context (Float,Float)

expression = "filter even [1,2,3,4]"
       

decls =
    case Parser.run Haskell.declList prelude of
        Ok l -> l
        Err _ -> []

functions = Eval.collectFunctions decls Eval.primitives

init : String -> (Model, Cmd msg)            
init str =
    ({ expression =
          case Parser.run Haskell.topExprEnd str of
              Result.Ok expr -> expr
              Result.Err _ -> Fail "parse error"
     , clientPos = (0,0)
     }
    , Cmd.none
    )

      
view : Model -> Html Msg
view model =
    div [class "lines"]
        [
         renderExpr model.expression Context.hole
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Eval ctx newPos ->
            if newPos /= model.clientPos then
                let
                    -- subExpr = Debug.log "eval: " (.getOption ctx model.expression)
                    newExpr = Maybe.withDefault model.expression
                              (Eval.redexCtx functions model.expression ctx)
                in 
                    ({ expression = newExpr
                     , clientPos = newPos
                     }
                    , Cmd.none)
            else
                (model, Cmd.none)

subscriptions : Model -> Sub msg
subscriptions _ = Sub.none

                    
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

                                    
renderExpr : Expr -> Context -> Html Msg
renderExpr expr ctx = renderExpr_ expr ctx 1 

 -- worker function with precedence level                       
renderExpr_ : Expr -> Context -> Int -> Html Msg
renderExpr_ expr ctx prec 
    = case expr of
          Var x ->
              text x

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
                  prec1 = getPrecedence ":"
                  ctx0 = Monocle.compose ctx Context.cons0
                  ctx1 = Monocle.compose ctx Context.cons1
              in
                  paren (prec > prec1)
                      <| span [ Mouse.onClick (\ev -> Eval ctx ev.clientPos) ]
                          [ renderExpr_ e0 ctx0 prec1
                          , text ":"
                          , renderExpr_ e1 ctx1 prec1
                          ]

          InfixOp op e0 e1 ->
              let
                  prec1 = getPrecedence op
                  ctx0 = Monocle.compose ctx Context.infixOp0
                  ctx1 = Monocle.compose ctx Context.infixOp1
              in
                  paren (prec > prec1)
                      <| span [ Mouse.onClick (\ev -> Eval ctx ev.clientPos) ]
                          [ renderExpr_ e0 ctx0 prec1
                          , text op
                          , renderExpr_ e1 ctx1 prec1
                          ]

          App e0 args ->
              let
                  n = List.length args - 1
                  ctx0 = Monocle.compose ctx Context.app0
                  ctxs = List.map (\i -> Monocle.compose ctx (Context.appArg i))
                         (List.range 0 n)
                  items = List.intersperse (text " ")
                          <| List.map2 (\ei ctxi -> renderExpr_ ei ctxi 10) args ctxs
                              
              in
                  paren (prec > 9)
                  <| span [ Mouse.onClick (\ev -> Eval ctx ev.clientPos) ]
                      <| renderExpr e0 ctx0 :: text " " :: items
                  
          Lam xs e1 ->
              let
                  items = List.intersperse (text " ") <| List.map text xs
              in paren (prec > 1)
                  <| span [] <| (text "\\" :: items) ++ [text " -> ", renderExpr e1 ctx]
                  
          IfThenElse e1 e2 e3 ->
              paren (prec > 1)
              <| span [ Mouse.onClick (\ev -> Eval ctx ev.clientPos) ]
                  [ text "if "
                  , renderExpr e1 (Monocle.compose ctx Context.if0)
                  , text " then "
                  , renderExpr e2 ctx
                  , text " else "
                  , renderExpr e3 ctx
                  ]
                  
          Fail msg -> span [] [ text "!"
                              , text msg ]


getPrecedence : Name -> Int    
getPrecedence op
    = case Dict.get op precedence of
          Just p -> p
          Nothing -> 1

precedence : Dict Name Int
precedence
    = Dict.fromList
      [ ("==", 4), ("/=", 4)
      , (":", 5), ("++", 5)
      , ("+", 6), ("-", 6)
      , ("*", 7)
      ]
                     

                      
paren : Bool -> Html msg -> Html msg
paren b html
    = --if b then
          span [] [text "(", html, text ")"]
      --else
      --    html        

        

