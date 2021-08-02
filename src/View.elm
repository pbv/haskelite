
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
import Html.Attributes exposing (class)
import Html.Events.Extra.Mouse as Mouse
import Platform.Cmd as Cmd
import Platform.Sub as Sub

import Dict exposing (Dict)
import Browser
import Debug


type alias Model
    = { expression : Expr              -- current expression
      , previous : List (Expr, String) -- list of previous steps
      , clientPos : (Float,Float)      -- last mouse click position (for debouncing)
      }

type Msg
    = Eval Context (Float,Float)


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
    ({ expression =
          case Parser.run Haskell.topExprEnd str of
              Result.Ok expr -> expr
              Result.Err _ -> Fail "parse error"
     , previous = []
     , clientPos = (0,0)
     }
    , Cmd.none
    )

      
view : Model -> Html Msg
view model =
    div [class "lines"]
        <| (List.map lineView (List.reverse model.previous) ++
                [
                 div [class "line"] [
                      renderExpr model.expression Context.hole
                     ]
                ]
           )

lineView : (Expr, String) -> Html Msg
lineView (expr,info) =
    div [] [
         div [class "line"] [ text (Pretty.prettyExpr expr) ],
         div [class "info"] [ text "{ ", text info, text " }" ]
        ]

            
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Eval ctx newPos ->
            if newPos /= model.clientPos then
                case Eval.redexCtx functions model.expression ctx of
                    Just (newExpr, info) ->
                        ({ expression = newExpr
                         , previous = (model.expression, info) :: model.previous
                         , clientPos = newPos
                         }
                        , Cmd.none
                        )
                             
                    Nothing -> ( { model | clientPos = newPos }
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
renderExpr expr ctx = renderExpr_ expr ctx  

 -- worker function 
renderExpr_ : Expr -> Context -> Html Msg
renderExpr_ expr ctx 
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
                  ctx0 = Monocle.compose ctx Context.cons0
                  ctx1 = Monocle.compose ctx Context.cons1
              in
                  paren <|
                      span (redexStyle expr ctx)
                      [ renderExpr_ e0 ctx0 
                      , text ":"
                      , renderExpr_ e1 ctx1 
                      ]

          InfixOp op e0 e1 ->
              let
                  ctx0 = Monocle.compose ctx Context.infixOp0
                  ctx1 = Monocle.compose ctx Context.infixOp1
              in
                  paren <|
                      span (redexStyle expr ctx)
                      [ renderExpr_ e0 ctx0 
                      , text op
                      , renderExpr_ e1 ctx1 
                      ]

          App e0 args ->
              let
                  n = List.length args - 1
                  ctx0 = Monocle.compose ctx Context.app0
                  ctxs = List.map (\i -> Monocle.compose ctx (Context.appArg i))
                         (List.range 0 n)
                  items = List.intersperse (text " ")
                          <| List.map2 (\ei ctxi -> renderExpr_ ei ctxi) args ctxs
                              
              in
                  paren <|
                      span (redexStyle expr ctx)
                      <| renderExpr e0 ctx0 :: text " " :: items
                  
          Lam xs e1 ->
              text (Pretty.prettyExpr expr)
                  {-
              let
                  items = List.intersperse (text " ") <| List.map text xs
              in paren <|
                  span [] <| (text "\\" :: items) ++ [text " -> ", renderExpr e1 ctx] -}
                  
          IfThenElse e1 e2 e3 ->
              paren <|
              span (redexStyle expr ctx)
                  [ text "if "
                  , renderExpr e1 (Monocle.compose ctx Context.if0)
                  , text " then "
                  , text (Pretty.prettyExpr e2)
                  , text " else "
                  , text (Pretty.prettyExpr e3) 
                  ]
                  
          Fail msg -> span [] [ text "!"
                              , text msg ]


redexStyle : Expr -> Context -> List (Html.Attribute Msg)
redexStyle expr ctx =
    if Eval.reduces functions expr then
        [ class "redex"
        , Mouse.onClick (\ev -> Eval ctx ev.clientPos) ]
    else []
        
                      
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
                     

                      
paren : Html msg -> Html msg
paren html
    =  span [] [text "(", html, text ")"]

        

