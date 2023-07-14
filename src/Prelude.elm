{-
  Standard definitions for Haskelite
  This is a subset of the Haskell 98 prelude

  Pedro Vasconcelos, 2021-23
-}
module Prelude exposing (preludeResult)

import AST exposing (Bind)
import HsParser
import Parser

-- result of parsing the prelude text below
preludeResult : Result String (List Bind)
preludeResult
    = Result.mapError HsParser.deadEndsToString <|
      Parser.run HsParser.declarations prelude 
                 
prelude : String
prelude =
    """
otherwise :: Bool
otherwise = True

(&&) :: Bool -> Bool -> Bool
False && _ = False
True  && x = x

(||) :: Bool -> Bool -> Bool
False || x = x
True  || _ = True

not :: Bool -> Bool
not True = False
not False = True

even :: Int -> Bool
even x = x `mod` 2 == 0

odd :: Int -> Bool
odd x = x `mod` 2 == 1

min :: Int -> Int -> Int
min x y | x<=y = x
        | otherwise = y

max :: Int -> Int -> Int
max x y | x<=y = y
        | otherwise = x

fst :: (a,b) -> a
fst (x,_) = x

snd :: (a,b) -> b
snd (_,y) = y

null :: [a] -> Bool
null [] = True
null (_:_) = False

head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

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

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

repeat :: a -> [a]
repeat x = x:repeat x

cycle :: [a] -> [a]
cycle xs = xs ++ cycle xs

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

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
filter f (x:xs) | f x = x : filter f xs 
                | otherwise = filter f xs

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
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

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f !z [] = z
foldl' f !z (x:xs) = foldl' f (f z x) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs) | p x = x : takeWhile p xs
                   | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:xs) | p x = dropWhile p xs
                   | otherwise = x:xs

enumFrom :: Int -> [Int]
enumFrom !n = n : enumFrom (n+1)

enumFromTo :: Int -> Int -> [Int]
enumFromTo i j | i<=j = i : enumFromTo (i+1) j
               | otherwise = []

enumFromThen :: Int -> Int -> [Int]
enumFromThen !n0 !n1 = n0 : enumFromThen n1 (2*n1-n0) 

enumFromThenTo :: Int -> Int -> Int -> [Int]
enumFromThenTo n0 n1 k | n0<=k = n0 : enumFromThenTo n1 (2*n1-n0) k
                       | otherwise = []
"""
