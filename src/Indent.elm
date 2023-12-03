module Indent exposing (list)

{-| This library provides a parser for indented text.
  | Based on https://github.com/eelcoh/parser-indent
-}

import Parser as P exposing ((|.), (|=), Parser)


-- | Parse a list with the same indentation, for a given parser.
-- the 2nd argument is the error message for incorrect indentation
--
list : Parser a -> String -> Parser (List a)
list parser msg =
    let
        list_ : ( Int, Int ) -> Parser (List a)
        list_ ( minimalIndent, actualIndent ) =
            if actualIndent > minimalIndent then
                P.withIndent actualIndent parser_
            else
                P.succeed []

        parser_ =
            P.succeed identity
                |= P.loop [] (step parser msg)
    in
    P.oneOf
        [ P.succeed (\a b -> ( a, b ))
            |. P.spaces
            |= P.getIndent
            |= P.getCol
            |> P.andThen list_
        , P.succeed []
            |. P.chompWhile (\c -> c == ' ')
            |. P.end
        ]


step : Parser a -> String -> List a -> Parser (P.Step (List a) (List a))
step parser msg values =
    let
        finish =
            P.Done (List.reverse values)

        next value_ =
            P.Loop (value_ :: values)
    in
    indented
        { smaller =
            P.succeed finish
        , exactly =
            P.oneOf
                [ P.succeed next
                    |= parser
                , P.succeed finish -- for statements on the same indentation level as the parent record
                ]
        , larger =
            P.problem msg
        , ending =
            P.succeed finish
        }


type alias NextParser a =
    { smaller : P.Parser a
    , exactly : P.Parser a
    , larger : P.Parser a
    , ending : P.Parser a
    }


indented : NextParser a -> P.Parser a
indented next =
    let
        proceed : ( Int, Int ) -> P.Parser a
        proceed ( minimal, actual ) =
            P.oneOf
                [ P.andThen (\_ -> next.ending) P.end
                , if actual == minimal then
                    next.exactly

                  else if actual > minimal then
                    next.larger

                  else
                    next.smaller
                ]
    in
    P.succeed (\a b -> ( a, b ))
        |= P.getIndent
        |. P.spaces
        |= P.getCol
        |> P.andThen proceed



-- Utils


spaces : Parser ()
spaces =
    -- Parser's implementation of spaces also takes newlines and carriage returns.
    -- Which is fine, but not if all you want it to do is chomp actual spaces.
    P.chompWhile (\c -> c == ' ')
