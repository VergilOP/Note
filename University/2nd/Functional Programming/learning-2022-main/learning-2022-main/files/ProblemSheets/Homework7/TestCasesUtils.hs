{-|
  This module contains the helper functions for writing test cases.
-}

module TestCasesUtils where

import Control.DeepSeq
import Data.List
import Test.QuickCheck

infix 4 ~=

{-|
  Evaluates strictly on the arguments and compares them using '=='.
  Make sure to put student's answer as the first argument in order to
  get an appropriate message for the counterexample.
-}
(~=) :: (Eq a, Show a, NFData a) => a -> a -> Property
x ~= y =
  counterexample ("Your output was " ++ show x ++ ", but the correct output is " ++ show y ++ ".")
  (deepseq x x == deepseq y y)

{-|
  Checks equality up to permuation using 'sort'.
-}
permOf :: (Eq a, Ord a) => [a] -> [a] -> Bool
permOf xs ys = sort xs == sort ys

{-|
  Evaluates strictly on the arguments and checks their equality up to permutation.
  Make sure to put student's answer as the first argument in order to
  get an appropriate message for the counterexample.
-}
p_permOf :: (Eq a, Ord a, Show a, NFData a) => [a] -> [a] -> Property
p_permOf xs ys =
  counterexample ("Your output " ++ show xs ++ " is not a permutation of " ++ show ys ++ ".")
  (deepseq xs xs `permOf` deepseq ys ys)

{-|
  Checks equality up to permuation by using a custom compare function.
-}
permOfBy :: (Eq a, Ord a) => (a -> a -> Bool) -> [a] -> [a] -> Bool
permOfBy eq xs ys = case (sort xs, sort ys) of
  ([], []) -> True
  (x:xs, y:ys) -> x `eq` y && permOfBy eq xs ys
  (_, _) -> False

{-|
  Evaluates strictly on the arguments and checks their equality up to permutation
  by using a custom compare function.
  Make sure to put student's answer as the first argument in order to
  get an appropriate message for the counterexample.
-}
p_permOfBy :: (Eq a, Ord a, Show a, NFData a) => (a -> a -> Bool) -> [a] -> [a] -> Property
p_permOfBy eq xs ys =
  counterexample ("Your output " ++ show xs ++ " is not a permutation of " ++ show ys ++ ".")
  (permOfBy eq (deepseq xs xs) (deepseq ys ys))

{-|
  Evaluates strictly on the arguments and checks if the first string is an infix of the second.
  Make sure to put student's answer as the first argument in order to
  get an appropriate message for the counterexample.
-}
p_isInfixOf :: String -> String -> Property
p_isInfixOf x y = counterexample ("Your output " ++ x ++ " is not a substring of " ++ y ++ ".")
                  (deepseq x x `isInfixOf` deepseq y y)

{-|
  Evaluates strictly on the arguments and checks if the second string is an infix of the first.
  Make sure to put student's answer as the first argument in order to
  get an appropriate message for the counterexample..
-}
p_contain :: String -> String -> Property
p_contain x y = counterexample ("Your output " ++ x ++ " does not contain " ++ y ++ ".")
                (deepseq y y `isInfixOf` deepseq x x)

{-|
  Evaluates strictly on the arguments and checks if the first argument is an element of the second.
  Make sure to put student's answer as the first argument in order to
  get an appropriate message for the counterexample.
-}
p_elem :: (Eq a, Show a, NFData a) => a -> [a] -> Property
p_elem x xs = counterexample ("Your output " ++ show x ++ " is not a member of " ++ show xs ++ ".")
             (deepseq x x `elem` deepseq xs xs)

{-|
  Checks if the first list is a subset of the second.
-}
subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs

{-|
  Evaluates strictly on the arguments and checks if the first list is a subset of the second.
  Make sure to put student's answer as the first argument in order to
  get an appropriate message for the counterexample.
-}
p_subset :: (Eq a, Show a, NFData a) => [a] -> [a] -> Property
p_subset xs ys = counterexample ("Your output " ++ show xs ++ " is not a subset of " ++ show ys ++ ".")
                 (xs `deepseq` ys `deepseq` (xs `subset` ys))

{-|
  Evaluates strictly on the arguments and checks if the first list is a superset of the second.
  Make sure to put student's answer as the first argument in order to
  get an appropriate message for the counterexample.
-}
p_superset :: (Eq a, Show a, NFData a) => [a] -> [a] -> Property
p_superset xs ys = counterexample ("Your output " ++ show xs ++ " is not a superset of " ++ show ys ++ ".")
                   (xs `deepseq` ys `deepseq` (ys `subset` xs))

{-|
  Evaluates strictly on the arguments and compares set equality.
  Make sure to put student's answer as the first argument in order to
  get an appropriate message for the counterexample.
-}
p_seteq :: (Eq a, Show a, NFData a) => [a] -> [a] -> Property
p_seteq xs ys = counterexample msg1 test1 .||. counterexample msg2 test2
  where
    xs' = deepseq xs xs
    ys' = deepseq ys ys
    msg1  = "Your output included the element "
            ++ show (head ([x | x <- xs' , not (x `elem` ys')]))
            ++ " which it should not have included."
    msg2  = "Your output did not include the element "
            ++ show (head ([y | y <- ys' , not (y `elem` xs')]))
            ++ " which it should have included."
    test1 = xs' `subset` ys'
    test2 = ys' `subset` xs'
  {- counterexample ("Your output\n" ++ show xs ++ "\nis not the same set as\n"
                     ++ show ys ++ ".")
     (xs `deepseq` ys `deepseq` (ys `subset` xs && xs `subset` ys)) -}

{-|
  Checks if the first list is an interleaving of the two lists in the second argument.
-}
isInterleave :: Eq a => [a] -> ([a], [a]) -> Bool
isInterleave [] ([], []) = True
isInterleave zs ([], ys) = ys == zs
isInterleave zs (xs, []) = xs == zs
isInterleave (z:zs) (x:xs, y:ys) =
  (x == z && isInterleave zs (xs, (y:ys))) || (y == z && isInterleave zs ((x:xs), ys))

{-|
  Evaluates strictly on the arguments and checks if zs is an interleaving of (xs, ys).
  Make sure to put student's answer as the first argument in order to
  get an appropriate message for the counterexample.
-}
p_isInterleave :: (Eq a, Show a, NFData a) => [a] -> ([a], [a]) -> Property
p_isInterleave zs (xs, ys) =
  counterexample ("Your output " ++ show zs ++ " is not an interleaving of (" ++ show xs ++ ", " ++ show ys ++ ").")
  (xs `deepseq` ys `deepseq` zs `deepseq` (zs `isInterleave` (xs, ys)))
