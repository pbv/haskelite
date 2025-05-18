module Tests exposing (..)

import Language exposing (..)
import Parser


example1 : String
example1 =
    """
const :: a -> b -> a
const x y = x              
     
id :: a -> a
id x = x           
     """

example2 : String
example2 =
    """
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \\x -> f (g x)

($) :: (a -> b) -> a -> b
f $ x = f x     
     """

example3 : String
example3 =         
    """enumFromThenTo :: Int -> Int -> Int -> [Int]
enumFromThenTo n0 n1 k
    | step>=0 = up n0 
    | otherwise = down n0
    where step = n1-n0
          up n | n<=k = n : up (n+step)
               | otherwise = []
          down n | n>=k = n : down (n+step)
                 | otherwise = []
    """        

example4 : String
example4 = """         
($) :: (a -> b) -> a -> b
f $ x = f x
        """
