{-
  Standard definitions for Haskelite
  This is a subset of the Haskell 98 prelude

  Pedro Vasconcelos, 2021-24
-}
module Prelude exposing (prelude)

import AST exposing (Module, Bind)
import Parser
import HsParser exposing (toplevelModule)
import Typecheck exposing (KindEnv, TyEnv, tcModule, initialKindEnv, initialTypeEnv)
import Tc 

-- result of parsing and typechecking the prelude text below
preludeModule : Result String Module
preludeModule
    = Result.mapError HsParser.deadEndsToString <|
      Parser.run toplevelModule preludeText 


prelude : Result String (List Bind, KindEnv, TyEnv)
prelude = preludeModule |>
          Result.andThen
              (\mod -> Tc.eval (tcModule initialKindEnv initialTypeEnv mod) |>
                   Result.andThen (\(kenv,tenv) -> Ok (mod.binds, kenv, tenv)))
          
preludeText : String
preludeText =
    """
-- Basic algebraic data types
data Bool = True | False

data Maybe a = Nothing | Just a

data Either a b = Left a | Right b

data Ordering = LT | EQ | GT

-- Prelude bindings
undefined :: a
undefined = error "undefined"

id :: a -> a
id x = x

const :: a -> b -> a
const x y = x

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

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

-- NB: these are too polymorphic!
min :: a -> a -> a
min x y | x<=y = x
        | otherwise = y

max :: a -> a -> a
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
head [] = error "head: empty list"
head (x:_) = x

tail :: [a] -> [a]
tail [] = error "tail: empty list"
tail (_:xs) = xs

length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x:(xs++ys)

reverse :: [a] -> [a]
reverse xs = reverseAcc xs []

reverseAcc [] acc = acc
reverseAcc (x:xs) acc = reverseAcc xs (x:acc)

sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

product :: [Int] -> Int
product [] = 1
product (x:xs) = x * product xs

-- NB: these are overly polymorphic!
maximum :: [a] -> a
maximum [] = error "maximum: empty list"
maximum [x] = x
maximum (x:xs) = max x (maximum xs)

minimum :: [a] -> a
minimum [] = error "minimum: empty list"
minimum [x] = x
minimum (x:xs) = min x (minimum xs) 

take :: Int -> [a] -> [a]
take n xs | n<=0 = []
take n [] = []
take n (x:xs) = x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop n xs | n<=0 = xs
drop n [] = []
drop n (x:xs) = drop (n-1) xs

(!!) :: [a] -> Int -> a
_ !! n | n<0 = error "!!: negative index"
[] !! _ = error "!!: index too large"
(x:_) !! 0 = x
(_:xs) !! n = xs !! (n-1)

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat xss

repeat :: a -> [a]
repeat x = let xs = x:xs 
           in xs

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

cycle :: [a] -> [a]
cycle [] = error "cycle: empty list"
cycle xs = let xs' = xs++xs' 
           in xs'

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and xs

or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs

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

-- from Data.List
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z [] = z
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
enumFromThen !n0 !n1 = n0 : enumFromThen n1 (n1+n1-n0)

enumFromThenTo :: Int -> Int -> Int -> [Int]
enumFromThenTo n0 n1 k 
    | step>=0 = up n0 
    | otherwise = down n0
    where step = n1-n0
          up n | n<=k = n : up (n+step)
               | otherwise = []
          down n | n>=k = n : down (n+step)
                 | otherwise = []
                      
lookup :: a -> [(a,b)] -> Maybe b
lookup _ [] = Nothing
lookup x ((y,v):rest) | x == y = Just v
                      | otherwise = lookup x rest

-- from Data.List
init :: [a] -> [a]
init [] = error "init: empty list"
init [_] = []
init (x:xs) = x : init xs

last :: [a] -> a
last [] = error "last: empty list"
last [x] = x
last (_:xs) = last xs
"""
