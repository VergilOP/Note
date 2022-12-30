-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module ClassTest2Catchup (stateF, runStateF, writeNodes, subtree, isMagicSquare, countNulls) where

import Data.List
import Data.Char

import Control.Monad.State
import Control.Monad.Writer

import Types

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}

stateF :: Integer -> State (Integer,Integer,Integer) ()
stateF = undefined

runStateF :: Integer -> Integer
runStateF n = let ((),(a,b,c)) = (runState (stateF n) (0,1,1)) in a

{- Question 2 -}

writeNodes :: T a  -> Writer [Maybe a] ()
writeNodes = undefined

{- Question 3 -}

subtree :: T a -> Address -> Maybe (T a)
subtree = undefined 

{- Question 4 -}

isMagicSquare :: [[Int]] -> Bool
isMagicSquare = undefined

{- Question 5 -}

countNulls :: Json -> State Integer () 
countNulls = undefined
