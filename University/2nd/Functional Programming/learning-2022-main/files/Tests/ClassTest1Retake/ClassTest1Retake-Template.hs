-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module ClassTest1Retake (checkPeriodic, divisibleByIndex, findCubes, edit, edits, solvable) where

import Data.List
import Data.Char

import Types

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}
checkPeriodic :: String -> Int -> Bool
checkPeriodic = undefined

{- Question 2 -}
divisibleByIndex :: [Int] -> [Bool]
divisibleByIndex = undefined

{- Question 3 -}
findCubes :: Int -> [(Int,Int,Int)]
findCubes = undefined

{- Question 4 -}
edit :: EditCommand -> Text -> Text
edit = undefined

edits :: [EditCommand] -> Text -> Text
edits = undefined
        
{- Question 5 -}
solvable :: ([Bool] -> Bool) -> Int -> Bool
solvable = undefined 

