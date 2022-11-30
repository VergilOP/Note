-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module PracticeTestSolutions (checksum , golfScorer, highlyDivisible, largestOddFactor, equals, babylonianPalindromes) where

import Types

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}
checksum :: Integral a => [a] -> Bool
checksum xs = length xs == 8 && sum xs `mod` 11 == 0

{- Question 2 -}
golfScorer :: Integer -> Integer -> Integer
golfScorer par strokes
 | strokes ==  1       = 5
 | diff    <= -2       = 4
 | diff    == -1       = 3
 | diff    ==  0       = 2
 | diff    ==  1       = 1
 | otherwise           = 0
 where diff = (strokes - par)

{- Question 3 -}
highlyDivisible :: Int -> [Int]
highlyDivisible n = take n [ i | i <- [1..] , allDivides i ]
  where allDivides :: Int -> Bool
        allDivides n = all (\ k -> n `mod` k == 0) [2..12]

largestOddFactor :: Int -> [Int]
largestOddFactor n = [ head (filter odd (reverse (factors i))) | i <- [1..n] ]

{- Question 4 -}

equals :: (Enum a, Bounded a, Eq b) => (a -> b) -> (a -> b) -> Bool
equals f g = and [ f x == g x | x <- [minBound .. maxBound] ]

{- Question 5 -}

toBase60 :: Integer -> [Integer]
toBase60 n = go n []
  where go n soFar | n < 60 = n:soFar
        go n soFar | otherwise = let r = n `mod` 60
                                     m = n - r
                                  in go (m `div` 60) (r:soFar)

babylonianPalindromes :: [Integer]
babylonianPalindromes =
  [ i | i <- [1..], let b = toBase60 i, length b > 1, b == reverse b ]
