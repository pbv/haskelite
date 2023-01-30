
# Haskelite

A single-step interpreter for a small subset of the Haskell language
including integers, booleans, lists, tuples and
recursive definitions. The principal focus is on simplicity since this
is intended for teaching fundamentals of functional programming.

This project is an extended re-implementation in Elm of the [Lambda
Lessons](https://stevekrouse.com/hs.js/) by Jan Paul Posma and Steve Krouse.

## Language features

### Expressions

Integers, variables, arithmetic operations (`+, -, *, div, mod`),
comparisons between integers (`==`, `<=`, etc.), tuples, list enumerations
(e.g. `[1..10]`) lambda-expressions and if-then-else.

### Function definitions

Pattern matching over integers, lists and tuples; recursive
definitions are allowed.


## Standard Prelude

Definitions for the following functions from the Standard Prelude are provided.

~~~haskell
even, odd :: Int -> Bool
max, min :: Int -> Int -> Int
fst :: (a,b) -> a
snd :: (a,b) -> b
(&&), (||) :: Bool -> Bool
head :: [a] -> a
tail :: [a] -> [a]
(++) :: [a] -> [a] -> [a]
length :: [a] -> Int
reverse :: [a] -> [a]
sum, product :: [Int] -> Int
take, drop :: Int -> [a] -> [a]
concat :: [[a]] -> [a]
repeat :: a -> [a]
cycle :: [a] -> [a]
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

The following Haskell 98 features are *not* implemented:

* ~~static type checking~~ type classes 
* the numeric tower (Float, Integer, Double, etc.)
* let and case expressions
* guards
* list compreensions
* caracters and strings
* user-defined algebraic data types

----

Pedro Vasconcelos, 2023.
