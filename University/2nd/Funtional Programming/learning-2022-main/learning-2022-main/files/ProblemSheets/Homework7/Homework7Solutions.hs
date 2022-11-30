-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Homework7Solutions (phoneToString , stringToPhone , fingerTaps) where

import Types

import Data.Char

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 7a -}
phoneKeyboard :: Button -> [Char]
phoneKeyboard '1' = "1"
phoneKeyboard '2' = "ABC2"
phoneKeyboard '3' = "DEF3"
phoneKeyboard '4' = "GHI4"
phoneKeyboard '5' = "JKL5"
phoneKeyboard '6' = "MNO6"
phoneKeyboard '7' = "PQRS7"
phoneKeyboard '8' = "TUV8"
phoneKeyboard '9' = "WXYZ9"
phoneKeyboard '0' = " 0"
phoneKeyboard '#' = ".,"
phoneKeyboard  _  = undefined -- avoid warnings

phoneToString :: [(Button, Presses)] -> Text
phoneToString bps = phoneToStringHelper bps False

-- The second argument indicates whether we need to capitalize or not.
phoneToStringHelper :: [(Button, Presses)] -> Bool -> Text
phoneToStringHelper []               _ = []
phoneToStringHelper (('*', p) : bps) _ = phoneToStringHelper
                                          bps ((p `mod` 2) == 1)
phoneToStringHelper ((b  , p) : bps) u = if   u
                                         then c         : r
                                         else toLower c : r
  where
    cs = phoneKeyboard b
    n  = length cs
    c  = cs !! ((p - 1) `mod` n)
    r  = phoneToStringHelper bps False

{- Question 7b -}
stringToPhone :: Text -> [(Button, Presses)]
stringToPhone = concatMap charToPhone

charToPhone :: Char -> [(Button, Presses)]
charToPhone c = if   isUpper c
                then ('*',1) : [(b,p)]
                else           [(b,p)]
  where
    -- Do brute force search to avoid writing everything out by hand :)
    (b,p) = head [(b',p'+1) | b' <- '#' : ['0'..'9']    ,
                              let cs = phoneKeyboard b' ,
                              let n  = length cs - 1    ,
                              p' <- [0..n]              ,
                              toUpper c == cs !! p']

{- Question 7c -}
fingerTaps :: Text -> Presses
fingerTaps t = sum (map snd (stringToPhone t))


