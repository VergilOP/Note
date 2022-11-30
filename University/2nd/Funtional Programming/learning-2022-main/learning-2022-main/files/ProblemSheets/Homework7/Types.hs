-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

---------------------------------------------------------------------------------
-------------------------- DO **NOT** MODIFY THIS FILE --------------------------
---------------------------------------------------------------------------------

module Types where

-- Valid buttons are ['0'..'9']++['*','#']
type Button = Char

-- Valid presses are [1..]
type Presses = Int

-- Valid text consists of
-- ['A'..'Z']++['a'...'z']++['0'..'9']++['.',',',' ']
type Text = String

