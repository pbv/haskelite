
# Relambda

An interactive interpreter for a small subset of the Haskell language
intended for teaching with integers, booleans, lists, tuples and
recursive definitions.

This is an extended re-implementation in the Elm language of [Lambda
Lessons](https://stevekrouse.com/hs.js/).

## Language features

### Expressions

Integers, variables, arithmetic operations (`+, -, *, div, mod`),
comparisions between integers (`==`, `<=`, etc.), lists (written
`[1,2,3]` and `1:[2,3]`), tuples and if-then-else.

### Function definitions

Pattern matching over integers, lists and tuples; recursive definitions are allowed.


## Standard Prelude

Definitions for the following prelude functions are provided.

~~~haskell
even, odd :: Int -> Bool
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
any, all :: (a -> Bool) -> [a] -> Bool
~~~

## Not implemented

The following Haskell features are *not* implemented:

* type checking 
* let and case expressions
* guards
* list ranges and compreensions
* caracters and strings
* algebraic data types

----

Pedro Vasconcelos, 2021.
