-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
module ClassTest2 (dec2FactString , factString2Dec , treeByLevels, rotateLeft, rotateRight, leafIndices, treeToParens, parensToTree, phoneToString, stringToPhone, fingerTaps, facHelper, factorial', applyfuns, updateNodes, eval, run) where

import Types

import Data.Char
import Data.List

import Control.Monad.Except
import Control.Monad.State

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

-- Exercise 1

dec2FactString :: Integer -> String
dec2FactString n = undefined

factString2Dec :: String -> Integer 
factString2Dec str = undefined

-- Exercise 2

treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels = undefined

-- Exercise 3

rotateLeft,rotateRight :: Tree a -> Tree a

rotateLeft = undefined

rotateRight = undefined

-- Exercise 4

leafIndices :: BT a -> BT (Int,Int)
leafIndices = undefined

-- Exercise 5

treeToParens :: Tree' -> String
treeToParens = undefined

parensToTree :: String -> Tree'
parensToTree = undefined

-- Exercise 6

{- Question 6a -}
phoneToString :: [(Button, Presses)] -> Text
phoneToString = undefined

{- Question 6b -}
stringToPhone :: Text -> [(Button, Presses)]
stringToPhone = undefined

{- Question 6c -}
fingerTaps :: Text -> Presses
fingerTaps = undefined

-- Exercise 7

facHelper :: Integer -> State Integer ()
facHelper = undefined

factorial' :: Integer -> Integer
factorial' n = snd (runState (facHelper n) 1)

-- Exercise 8

applyfuns :: (a -> c) -> (b -> d) -> Tree'' a b -> Tree'' c d
applyfuns = undefined

-- Exercise 9

updateNodes :: Route -> (a -> a) -> BinTree a -> BinTree a
updateNodes = undefined

-- Exercise 10

eval :: MonadError String m => CalcExpr -> m Int
eval = undefined

run :: (MonadState Int m, MonadError String m) => CalcCmd -> m ()
run = undefined

