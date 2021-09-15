
# Haskelite

A single-step interpreter for a small subset of the Haskell language
intended for teaching with integers, booleans, lists, tuples and
recursive definitions. The principal focus is on simplicity since this
is intended for teaching fundamentals of functional programming.

This project is an extended re-implementation in Elm of the [Lambda
Lessons](https://stevekrouse.com/hs.js/).

## Language features

### Expressions

Integers, variables, arithmetic operations (`+, -, *, div, mod`),
comparisons between integers (`==`, `<=`, etc.), tuples,
lambda-expressions and if-then-else.

### Function definitions

Pattern matching over integers, lists and tuples; recursive
definitions are allowed.


## Standard Prelude

Definitions for the following prelude functions are provided.

~~~haskell
even, odd :: Int -> Bool
max, min :: Int -> Int -> Int
fst :: (a,b) -> a
snd :: (a,b) -> b
(&&), (||) :: Bool -> Bool
(++) :: [a] -> [a] -> [a]
length :: [a] -> Int
reverse :: [a] -> [a]
sum, product :: [Int] -> Int
take, drop :: Int -> [a] -> [a]
map :: (a -> b) -> [a] -> [b]
filter :: (a -> Bool) -> [a] -> [a]
foldr :: (a -> b -> b) -> b -> [a] -> b
foldl :: (a -> b -> a) -> a -> [b] -> a
zip :: [a] -> [b] -> [(a,b)]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
takeWhile, dropWhile :: (a -> Bool) -> [a] -> [a]
any, all :: (a -> Bool) -> [a] -> Bool
~~~

## Not implemented

The following Haskell features are *not* implemented:

* static type checking 
* the numeric tower (Float, Integer, Ratio, Complex, etc.)
* let and case expressions
* guards
* list ranges and compreensions
* caracters and strings
* user-defined algebraic data types

----

Pedro Vasconcelos, 2021.
