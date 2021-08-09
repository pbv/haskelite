
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
(&&) :: Bool -> Bool -> Bool
False && x = False
True  && x = x

(||) :: Bool -> Bool -> Bool
False || x = x
True  || x = True

not :: Bool -> Bool
not True = False
not False = True

even :: Int -> Bool
even x = mod x 2 == 0

odd :: Int -> Bool
odd x = mod x 2 == 1

min :: Int -> Int -> Int
min x y = if x<=y then x else y

max :: Int -> Int -> Int
max x y = if x<=y then y else x

fst :: (a,b) -> a
fst (x,y) = x

snd :: (a,b) -> b
snd (x,y) = y

head :: [a] -> a
head (x:xs) = x

tail :: [a] -> [a]
tail (x:xs) = xs

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

take :: Int -> [a] -> [a]
take 0 xs = []
take n [] = []
take n (x:xs) = x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop n [] = []
drop n (x:xs) = drop (n-1) xs

any :: (a -> Bool) -> [a] -> Bool
any f [] = False
any f (x:xs) = f x || any f xs

all :: (a -> Bool) -> [a] -> Bool
all f [] = True
all f (x:xs) = f x && all f xs

map :: (a->b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x:xs)= if f x then x : filter f xs else filter f xs

zip :: [a] -> [b] -> [(a,b)]
zip [] ys = []
zip xs [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] ys = []
zipWith f xs [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs
"""
