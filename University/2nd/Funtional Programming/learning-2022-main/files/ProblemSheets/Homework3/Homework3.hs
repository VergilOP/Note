-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}


module Homework3 (gasUsage , luhnDouble , luhn) where

import Types

{-Question 1-}

gasUsage :: (Fractional a, Ord a) => a -> Classification
gasUsage x
    | x < 3.0               = Low
    | 3.0 <= x && x < 5.0   = Medium
    | 5.0 <= x && x < 7.0   = High
    | 7.0 <= x              = SuperHigh

{-Question 2-}

luhnDouble :: Int -> Int
luhnDouble x
    | (2*x) > 9   = (2*x) - 9
    | otherwise   = 2*x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x1 x2 x3 x4
    | (luhnDouble x1 + x2 + luhnDouble x3 + x4) `mod` 10 == 0  = True
    | otherwise                                                 = False