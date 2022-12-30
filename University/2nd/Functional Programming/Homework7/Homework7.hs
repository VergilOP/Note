-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Homework7 (phoneToString , stringToPhone , fingerTaps) where

import Types

import Data.Char

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 7a -}
phoneToString :: [(Button, Presses)] -> Text
phoneToString [] = ""
phoneToString (('*',c):[]) = ""
phoneToString ((a,b):[]) = enterchar a b : ""
phoneToString (('*',c):(a,b):xs) = if c `mod` 2 == 1 then toUpper (enterchar a b):phoneToString xs else enterchar a b: phoneToString xs
phoneToString ((a,b):xs) = enterchar a b : phoneToString xs

enterchar :: Button -> Presses -> Char
enterchar y n  = xs !! realpress
    where buttonlist = zip ['1','2','3','4','5','6','7','8','9','#','0'] ["1","2abc","3def","4ghi","5jkl","6mno","7pqrs","8tuv","9wxyz",",.","0 "]
          (x,xs) = head (filter (\(a,b) -> a == y) buttonlist)
          realpress = n `mod` (length xs)

{- Question 7b -}
stringToPhone :: Text -> [(Button, Presses)]
stringToPhone [] = []
stringToPhone (x:[]) = tophone x ++ []
stringToPhone (x:xs) = tophone x ++ stringToPhone xs

tophone :: Char -> [(Button, Presses)]
tophone x
    | elem x (['a'..'z']++[',',' ','.']++['0']) = tonum x:[]
    | otherwise = ('*',1) : tonum (toLower x):[]
    
tonum :: Char -> (Button, Presses)
tonum n = y
    where buttonlist = zip "1abc2def3ghi4jkl5mno6pqrs7tuv8wxyz9 0.," (zip "1222233334444555566667777788889999900##" (map digitToInt "112341234123412341234123451234123451212"))
          (x,y) = head (filter (\(a,b) -> a == n) buttonlist)

{- Question 7c -}
fingerTaps :: Text -> Presses
fingerTaps str = sum (map (\(x,y)->y) (stringToPhone str))