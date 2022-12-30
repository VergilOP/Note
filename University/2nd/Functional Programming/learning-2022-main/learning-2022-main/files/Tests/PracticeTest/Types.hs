-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

{-# LANGUAGE FlexibleInstances #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

---------------------------------------------------------------------------------
-------------------------- DO **NOT** MODIFY THIS FILE --------------------------
---------------------------------------------------------------------------------

module Types where

import Control.Concurrent
import Data.Int
import Data.Maybe

-- Q3

factors :: Int -> [Int]
factors n = [ k | k <- [1..n]
                , n `mod` k == 0 
                ] 

-- Q4 

neg :: Int8 -> Int8
neg n = -n

doubleNeg :: Int8 -> Int8
doubleNeg n = - (- n)

