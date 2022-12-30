module Homework3Solutions where

import Types

gasUsage :: (Fractional a, Ord a) => a -> Classification
gasUsage g | g < 3           = Low
           | 3 <= g && g < 5 = Medium
           | 5 <= g && g < 7 = High
           | otherwise       = SuperHigh

luhnDouble :: Int -> Int
luhnDouble x = if 2 * x > 9 then 2 * x - 9 else 2 * x

-- Using where (to be discussed later), so we can store the result 2 * x.
luhnDouble' :: Int -> Int
luhnDouble' x = if y > 9 then y - 9 else y
  where
    y = 2 * x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (d + luhnDouble c + b + luhnDouble a) `mod` 10 == 0
