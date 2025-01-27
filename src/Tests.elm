module Tests exposing (..)

import Pretty exposing (Doc, a, string, line, group)

test : Int -> String
test n = Pretty.pretty 70 (fillwords (big n))

fillwords : List String -> Doc t
fillwords xs = Pretty.fold cat (List.map string xs)

cat : Doc t -> Doc t -> Doc t
cat x y = x |> a (group line) |> a y

big : Int -> List String
big n = List.map String.fromInt (List.range 1 n)
