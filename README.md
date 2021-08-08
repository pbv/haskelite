
# Relambda

An interactive interpreter for a small subset of the Haskell language
with integers, booleans, lists and recursive definitions intended for
teaching.

This is a extended re-implementation in Elm of [Lambda
Lessons](https://stevekrouse.com/hs.js/).

## Language features

Expressions

:  Integers, variables, arithmetic operations (`+, -, *, div, mod`),
comparisions between integers (`==`, `<=`, etc.), lists (written
`[1,2,3]` and `1:[2,3]:`), if-then-else

Definitions

:  Pattern matching over integers and lists; recursive definitions are allowed.


## Standard Prelude

Definitions for the following prelude functions are provided.

~~~haskell
even, odd :: Int -> Bool
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
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
any, all :: (a -> Bool) -> [a] -> Bool
~~~

## Not implemented

The following Haskell features are not implemented:

* type checking 
* let and case expressions
* tuples
* guards
* list ranges and compreensions
* caracters and strings
* algebraic data types

----

Pedro Vasconcelos, 2021.
