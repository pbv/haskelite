
# Haskelite

This a single-step interpreter for a subset of the Haskell language
including integers, booleans, lists, tuples and recursive
definitions. The principal focus is on simplicity since this is
intended for teaching fundamentals of functional programming.

This project is a re-implementation of the [Lambda
Lessons](https://stevekrouse.com/hs.js/) by Jan Paul Posma and Steve
Krouse in Elm and extended with more features, e.g.:

* strings and characters;
* full pattern matching and boolean guards;
* case expressions;
* let and where bindings;
* algebraic data type definitions;
* lazy evaluation;
* a more thorough implementation of the [Haskell
  Prelude](https://www.haskell.org/onlinereport/standard-prelude.html).

The interpreter is based on an abstract machine for a pattern matching
calculus described in a paper submitted to the IFL'2023 symposium.
Please contact me at <em>pbv at dcc dot fc dot up dot pt</em> if you
are interested in a draft copy.

The CodeView custom element is based on the [code published](https://github.com/billstclair/elm-custom-element/) by Bill St. Clair.

## Language features

Please check the supporte Haskell subset
[here](https://pbv.github.io/haskelite/site/language.html).

## Demo

[Try Haskelite now!](https://pbv.github.io/haskelite/site/index.html)

----

Pedro Vasconcelos, 2023.
