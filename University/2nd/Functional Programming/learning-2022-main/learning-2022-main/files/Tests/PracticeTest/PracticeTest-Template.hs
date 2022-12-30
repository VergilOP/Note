-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module PracticeTest (checksum , golfScorer, highlyDivisible, largestOddFactor, equals, babylonianPalindromes) where

import Types

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}
checksum :: Integral a => [a] -> Bool
checksum = undefined

{- Question 2 -}
golfScorer :: Integer -> Integer -> Integer
golfScorer = undefined

{- Question 3 -}
highlyDivisible :: Int -> [Int]
highlyDivisible = undefined

largestOddFactor :: Int -> [Int]
largestOddFactor = undefined
  
{- Question 4 -}
equals :: (Enum a, Bounded a, Eq b) => (a -> b) -> (a -> b) -> Bool
equals = undefined

{- Question 5 -}

babylonianPalindromes :: [Integer]
babylonianPalindromes = undefined



