
module Prelude exposing (..)

import AST exposing (Decl)
import Eval exposing (Functions)
import Haskell
import Parser

-- global declarations from the prelude      
declarations : List Decl
declarations =
    case Parser.run Haskell.declList prelude of
        Ok l -> l
        Err _ -> []  --  NB: this should never happen

functions : Functions                 
functions = Eval.collectFunctions declarations Eval.primitives


prelude : String
prelude =
    """
even :: Int -> Bool
even x = mod x 2 == 0

odd :: Int -> Bool
odd x = mod x 2 == 1

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x:(xs++ys)

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

product :: [Int] -> Int
product [] = 1
product (x:xs) = x * product xs

map :: (a->b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs)= if f x then x : filter f xs else filter f xs

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] ys = ys
zipWith f xs [] = xs
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs
"""
