-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE FlexibleContexts #-}

module Homework9 (applyfuns, updateNodes, eval, run) where

import Types

import Control.Monad.Except
import Control.Monad.State

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

-- Question 0

facHelper :: Integer -> State Integer ()
facHelper = undefined

factorial :: Integer -> Integer
factorial n = snd (runState (facHelper n) 1)

-- Question 1
applyfuns :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d
applyfuns = undefined

-- Question 2
updateNodes :: Route -> (a -> a) -> BinTree a -> BinTree a
updateNodes = undefined

-- Question 3
eval :: MonadError String m => CalcExpr -> m Int
eval = undefined

run :: (MonadState Int m, MonadError String m) => CalcCmd -> m ()
run = undefined
