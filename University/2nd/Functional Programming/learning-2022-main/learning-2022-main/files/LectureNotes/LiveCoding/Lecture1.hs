fac :: Integer -> Integer
fac n = if n == 0 
        then 1
        else n * fac (n-1)

from :: Integer -> [Integer]
from n = n : from (n+1)

-- In Java:
-- n==0 ? 1 : n*fac(n-1)
-- if_then_else_ :: Bool -> a -> a -> a

-- Haskell Curry

-- Non-standard
-- "curried functions"
myif :: Bool -> (a -> a -> a)
myif False x y = y
myif True x y = x

-- uncurried functions
myif' :: (Bool , a , a) -> a
myif' (False , x , y) = y
myif' (True , x , y) = x

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty (_:_) = False
-- [0,1,2]
-- head is 0  -- that would be x
-- tail is [1,2] -- this xs

foo :: [a] -> Int
foo [] = 0
foo (x:[]) = 1
foo (x:y:xs) = 2
-- x=0 y=1 ys=[2] for [0,1,2]

-- (a , b) -> c
-- a -> (b -> c)

add' :: Int -> (Int -> Int)
add' x = g
 where
  g :: Int -> Int
  g y = x + y