{-# LANGUAGE FlexibleInstances #-}
--
--  Lecture 3
--
--  Attendance:  41062320
--

-- Examples
--

-- class Eq a where
--   (==) : a -> a -> Bool

--  Instances of Eq
--
--    1. Int, Float, Double, Word, ...
--    2. Booleans
--    3. Char, Strings
--    4. [a] so long as a is
--
--  Non-instances
--
--    1. a -> b
--

--  Other useful/intesting typeclasses...
--
--  1. Ord   (<), (<=), ....
--  2. Show   show
--  3. Read   read

-- class Show a where
--   show : a -> String

-- class ----> interance
-- instance ---> a class which implements the interface

contains :: Eq a => a -> [a] -> Bool
contains x [] = False
contains x (y:ys) | x == y = True
contains x (y:ys) | otherwise = contains x ys

addAll :: Num a => [a] -> a
addAll [] = 0
addAll (x:xs) = x + addAll xs


class Mnd a where
  (<**>) :: a -> a -> a
  unit :: a

instance Mnd [a] where
  (<**>) = (++)
  unit = []

-- instance Mnd Int where
--   (<**>) = (*)
--   unit = 1

instance Mnd Int where
  (<**>) = (+)
  unit = 0

addAllMnd :: Mnd a => [a] -> a
addAllMnd [] = unit
addAllMnd (x:xs) = x <**> addAllMnd xs

instance Mnd (a -> a) where
  -- <**> :: (a -> a) -> (a -> a) -> (a -> a)
  (<**>) f g = (\ x -> f (g x))
  -- unit :: a -> a 
  unit = (\ x -> x)
  

instance (Mnd a, Mnd b) => Mnd (a,b) where
  (<**>) (x,y) (z,w) = (x <**> z , y <**> w)
  -- unit :: (a,b)
  unit = (unit, unit) 

--
--  List Comprehensions
--

-- [1,3,4]    - syntactic sugar ...
-- 1:3:4:[]   - ... for this

-- [ output | generators ]

numbers :: Int -> [Int]
numbers n = [ i | i <- [1..n]]

numbersPlusOne :: Int -> [Int]
numbersPlusOne n = [ i + 1 | i <- [1..n]]

pairs :: Int -> [(Int,Int)]
pairs n = [ (i,j) | i <- [1..n] , j <- [1..n] ]

increasingPairs :: Int -> [(Int,Int)]
increasingPairs n = [ (i,j) | i <- [1..n]
                            , j <- [i..n]
                            ]
--
--  for (i=1;i<n;i++) {
--    for (j=1;j<n;j++) {
--       lskdjfaldkfj
--    }
--  } 
--
evens :: Int -> [Int]
evens n = [ i | i <- [1..n] 
              , i `mod` 2 == 0
              ]
    
--
-- where exp1 is either
--   1. a generating expression
--        i <- l   with l a list
--   2. a boolean expression (a 'guard')
--   3. a let expression 
-- 
-- [ output | exp1
--          , exp2
--          , exp3
--          ]
--

factors :: Int -> [Int]
factors n = [ k | k <- [1..n]
                , n `mod` k == 0 
                ] 

isPrime :: Int -> Bool
isPrime n = factors n == [1,n]

--
--  a^2 + b^2 = c^2
--
pythagoreanTriples :: Int -> [(Int,Int,Int)]
pythagoreanTriples n = [ (i,j,k) | i <- [1..n]
                                 , j <- [i..n]
                                 , k <- [j..n]
                                 , i^2 + j^2 == k^2
                                 ] 

perfects :: Int -> [Int]
perfects n = [ i | i <- [1..n]
                 , sum (factors i) - i == i 
                 ]

