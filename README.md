
# Haskelite

This a single-step interpreter for a subset of the Haskell language
including integers, booleans, lists, tuples and recursive
definitions. The principal focus is on simplicity since this is
intended for teaching fundamentals of functional programming.

This project started as a re-implementation of the [Lambda
Lessons](https://stevekrouse.com/hs.js/) by Jan Paul Posma and Steve
Krouse in Elm and extended with more features, e.g.:

* strings and characters;
* full pattern matching and boolean guards;
* case expressions;
* let and where bindings;
* algebraic data type definitions;
* lazy evaluation;
* definitions for commonly used functions from the Haskell Prelude.

This interpreter and associated lazy abstract machine are
described in the paper [Haskelite: A Tracing Interpreter Based on a
Pattern-Matching Calculus](https://doi.org/10.1145/3677999.3678274)
(accept for publication in the proceedings of the *Haskell 2024 Symposium*).

The CodeView custom element is based on the [code published](https://github.com/billstclair/elm-custom-element/) by Bill St. Clair.

## Language features

Please check the supported subset of the Haskell language
[here](https://pbv.github.io/haskelite/site/language.html).

## Online Demo

[Try Haskelite now!](https://pbv.github.io/haskelite/site/index.html)

## Running locally

You can try the interpreter locally by opening the file
`site/index.html` using a web browser; several examples are also
available from this page. All the required HTML, CSS and JavaScript
files are bundled; no external files are downloaded and no internet
connection is needed.

## Re-compiling from source

The Elm source is in the `src` directory; some aditional open-source
JavaScript libraries are included in `site/js`.

To build the interpreter just run `make` from the main directory; this
will invoke `elm make` to download the Elm packages listed in
`elm.json` and compile all sources to a minimized `haskelite-min.js`
JS file.  The following tools need to be installed:

* the [Elm compiler](https://elm-lang.org/);
* the [uglyfy-js](https://www.npmjs.com/package/uglify-js) JavaScript
  minimizer (part of the NodeJS tools);
* a make tool (e.g. GNU make).

This software was developed in Ubuntu Linux with Firefox and tested
with Chrome, but should work in other OSes and browsers provided
they support reasonably recent CSS and JavaScript.


----

Pedro Vasconcelos, 2024.
