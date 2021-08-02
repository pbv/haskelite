
module Prelude exposing (..)

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

map :: (a->b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs)= if f x then x : filter f xs else filter f xs
"""
