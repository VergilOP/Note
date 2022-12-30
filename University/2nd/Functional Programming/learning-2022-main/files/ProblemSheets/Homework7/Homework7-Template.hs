-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Homework7 (phoneToString , stringToPhone , fingerTaps) where

import Types

import Data.Char
import Data.List

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 7a -}
phoneToString :: [(Button, Presses)] -> Text
phoneToString = undefined

{- Question 7b -}
stringToPhone :: Text -> [(Button, Presses)]
stringToPhone = undefined

{- Question 7c -}
fingerTaps :: Text -> Presses
fingerTaps = undefined

