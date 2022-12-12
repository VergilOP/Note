-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module ClassTest1-Solution (openOrSenior, rot13, digpow, sumDigPow, alphabetWar, xbonacci, encrypt, decrypt, wave, solve, abbreviate) where

import Types
import Data.Char

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

-- They are my solutions

{- Question 1 -}

-- https://www.codewars.com/kata/5502c9e7b3216ec63c0001aa

-- data Membership = Open | Senior deriving (Eq, Show)
openOrSenior :: [(Int, Int)] -> [Membership]
openOrSenior xs = [check x | x <- xs]

check :: (Int,Int) -> Membership
check (a,b)
    | a >= 55 && b > 7 = Senior
    | otherwise = Open

{- Question 2 -}

-- https://www.codewars.com/kata/530e15517bc88ac656000716

rot13 :: String -> String
rot13 str = map shift str

shift :: Char -> Char
shift c
    | not (elem c ['a'..'z']) && not (elem c ['A'..'Z']) = c
    | isLower c = chr (ord 'a' + ((ord c - ord 'a' + 13) `mod` 26))
    | otherwise = chr (ord 'A' + ((ord c - ord 'A' + 13) `mod` 26))

{- Question 3 -}

-- https://www.codewars.com/kata/5552101f47fc5178b1000050

digpow :: Integer -> Integer -> Integer
digpow n p = if y /= 0 then -1 else fromIntegral x
    where (x,y) = quotRem (sum (map (\(a,b) -> a^b) (zip (map digitToInt (show n)) [p..]))) (fromIntegral n) 

{- Question 4 -}

-- https://www.codewars.com/kata/5626b561280a42ecc50000d1

sumDigPow :: Int -> Int -> [Int]
sumDigPow a b = [x | x <- [a..b] , checkdig x]

checkdig :: Int -> Bool
checkdig x = sum (map (\(y,z) -> y^z) (zip (map digitToInt (show x)) [1..])) == x

{- Question 5 -}

-- https://www.codewars.com/kata/59377c53e66267c8f6000027

alphabetWar :: String -> String
alphabetWar = alphabetWar' 0 0

alphabetWar' :: Int -> Int -> String -> String
alphabetWar' a b []
    | a > b = "Left side wins!"
    | a < b = "Right side wins!"
    | otherwise = "Let's fight again!"
alphabetWar' a b (x:xs)
    | x == 'w' = alphabetWar' (a+4) b xs
    | x == 'p' = alphabetWar' (a+3) b xs
    | x == 'b' = alphabetWar' (a+2) b xs
    | x == 's' = alphabetWar' (a+1) b xs
    | x == 'm' = alphabetWar' a (b+4) xs
    | x == 'q' = alphabetWar' a (b+3) xs
    | x == 'd' = alphabetWar' a (b+2) xs
    | x == 'z' = alphabetWar' a (b+1) xs
    | otherwise = alphabetWar' a b xs

{- Question 6 -}

-- https://www.codewars.com/kata/556e0fccc392c527f20000c5

xbonacci :: Num a => [a] -> Int -> [a]
xbonacci as n = take n (as ++ (go as))
    where go s = sum s : go (tail s ++ [sum s])

{- Question 7 -}

-- https://www.codewars.com/kata/57814d79a56c88e3e0000786

encrypt :: String -> Int -> String
encrypt str n
    | n <= 0 = str
    | otherwise = encrypt (oddstr ++ evenstr) (n-1)
    where newstr = zip str [0..]
          oddstr = map (fst) (filter (\(x,y) -> odd y) newstr)
          evenstr = map (fst) (filter (\(x,y) -> even y) newstr)

decrypt :: String -> Int -> String
decrypt str n
    | n <= 0 = str
    | otherwise = decrypt (go fststr sndstr) (n-1)
    where len = (length str) `div` 2
          fststr = take len str
          sndstr = drop len str
          go [] [] = []
          go [] (x:[]) = [x]
          go (y:ys) (x:xs) = x:y:(go ys xs)

{- Question 8 -}

-- https://www.codewars.com/kata/58f5c63f1e26ecda7e000029

wave :: String -> [String]
wave str = go (length str) newstr
    where newstr = zip str [0..]
          go 0 _ = []
          go n nestr = if fst (nestr !! (length nestr - n)) == ' ' then go (n-1) nestr else map (\(a,b) -> if b == (length nestr - n) then toUpper a else a ) newstr : go (n-1) nestr

{- Question 9 -}

-- https://www.codewars.com/kata/59d9ff9f7905dfeed50000b0

solve :: [String] -> [Int]
solve xs = map (\ys -> go (map toLower ys) ['a'..'z']) xs
    where go [] _ = 0
          go (a:as) (b:bs) = if a == b then 1 + go as bs else go as bs

{- Question 10 -}

-- https://www.codewars.com/kata/5375f921003bf62192000746

abbreviate :: String -> String
abbreviate [] = [] 
abbreviate str = if b == [] then trans a else trans a ++ head b : abbreviate (tail b)
    where (a,b) = break (\x-> not (isLetter x)) str
          trans n
              | length n < 4 = n
              | otherwise = head n: show (length n - 2) ++ [last n]