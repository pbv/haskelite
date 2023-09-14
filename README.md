
# Haskelite

This project is a single-step interpreter for a small subset of the
Haskell language including integers, booleans, lists, tuples and
recursive definitions. The principal focus is on simplicity since this
is intended for teaching fundamentals of functional programming.

This project is an extended re-implementation in Elm of the [Lambda
Lessons](https://stevekrouse.com/hs.js/) by Jan Paul Posma and Steve Krouse.

The interpreter is based on an abstract machine for a
pattern matching calculus described in a
paper submitted to the IFL'2023 symposium.  Please contact me at
<em>pbv at dcc dot fc dot up dot pt</em> if you are interested in a
draft copy.

## Language features

### Expressions

Integers, characters, strings (lists of characters), variables,
arithmetic operations (`+, -, *, div, mod`), comparisons (`==`, `<=`,
etc.), tuples, lists, enumerations (e.g. `[1..10]`) and
lambda-expressions.

### Function definitions

Pattern matching over integers, characters, lists and tuples; boolean
guards; recursive definitions.

## Standard Prelude

Definitions for the following functions from the Standard Prelude are
provided.  Note that functions based on equality or ordering have an
overly polymorphic type (they should really have a typeclass
restriction) and thus can "get stuck" at runtime.

~~~haskell
even, odd :: Int -> Bool
max, min :: a -> a -> a         -- NB: too polymorphic!
fst :: (a,b) -> a
snd :: (a,b) -> b
(&&), (||) :: Bool -> Bool
head :: [a] -> a
tail :: [a] -> [a]
(++) :: [a] -> [a] -> [a]
length :: [a] -> Int
reverse :: [a] -> [a]
init :: [a] -> [a]
last :: [a] -> a
maximum, minimum :: [a] -> a    -- NB: too polymorphic!
sum, product :: [Int] -> Int
take, drop :: Int -> [a] -> [a]
concat :: [[a]] -> [a]
repeat :: a -> [a]
cycle :: [a] -> [a]
iterate :: (a->a) > a -> [a]
map :: (a -> b) -> [a] -> [b]
filter :: (a -> Bool) -> [a] -> [a]
foldr :: (a -> b -> b) -> b -> [a] -> b
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl' :: (a -> b -> a) -> a -> [b] -> a
zip :: [a] -> [b] -> [(a,b)]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
takeWhile, dropWhile :: (a -> Bool) -> [a] -> [a]
any, all :: (a -> Bool) -> [a] -> Bool
~~~

## Not implemented

The following Haskell 98 features are *not* implemented:

* modules
* type classes 
* the numeric tower (Float, Integer, Double, etc.)
* list compreensions
* user-defined algebraic data types

----

Pedro Vasconcelos, 2023.
