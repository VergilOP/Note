-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module ClassTest1 (openOrSenior, rot13, digpow, sumDigPow, alphabetWar, xbonacci, encrypt, decrypt, wave, solve, abbreviate) where

import Types
import Data.Char

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}

-- data Membership = Open | Senior deriving (Eq, Show)
openOrSenior :: [(Int, Int)] -> [Membership]
openOrSenior = undefined

{- Question 2 -}

rot13 :: String -> String
rot13 str = undefined

{- Question 3 -}

digpow :: Integer -> Integer -> Integer
digpow n p = undefined

{- Question 4 -}

sumDigPow :: Int -> Int -> [Int]
sumDigPow a b = undefined

{- Question 5 -}

alphabetWar :: String -> String
alphabetWar = undefined

{- Question 6 -}

xbonacci :: Num a => [a] -> Int -> [a]
xbonacci as n = undefined

{- Question 7 -}

encrypt :: String -> Int -> String
encrypt = undefined

decrypt :: String -> Int -> String
decrypt = undefined

{- Question 8 -}

wave :: String -> [String]
wave = undefined

{- Question 9 -}

solve :: [String] -> [Int]
solve = undefined

{- Question 10 -}

abbreviate :: String -> String
abbreviate = undefined