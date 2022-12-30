-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module ClassTest2 (stateTrib, runStateTrib, writeLeaves, collapse, mapLeavesWithAddress, toQuadTree, fromQuadTree) where

import Data.List
import Data.Char

import Control.Monad.State
import Control.Monad.Writer

import Types

-- Question 1

stateTrib :: Integer -> State (Integer,Integer,Integer) ()
stateTrib = undefined

runStateTrib :: Integer -> Integer
runStateTrib n =
  let ((),(a,b,c)) = runState (stateTrib n) (1,0,0)
  in a

-- Question 2

writeLeaves :: Bin a b -> Writer [Either a b] ()
writeLeaves = undefined

-- Question 3

collapse :: Bin (Bin a b) b -> Bin a b
collapse = undefined

-- Question 4

mapLeavesWithAddress :: (a -> Address -> c) -> Bin a b -> Bin c b
mapLeavesWithAddress = undefined

-- Question 5

toQuadTree :: Image -> QuadTree
toQuadTree = undefined

fromQuadTree :: QuadTree -> Image
fromQuadTree = undefined
