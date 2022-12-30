-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}


module Homework3 (gasUsage , luhnDouble , luhn) where

import Types

gasUsage :: (Fractional a, Ord a) => a -> Classification
gasUsage = undefined

luhnDouble :: Int -> Int
luhnDouble = undefined

luhn :: Int -> Int -> Int -> Int -> Bool
luhn = undefined

