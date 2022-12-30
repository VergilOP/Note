{-# LANGUAGE StandaloneDeriving #-}

module PracticeTestTestCases where

import MarkingCore

import TestCasesUtils
import qualified PracticeTest as Student
import qualified PracticeTestSolutions as Solutions

import Test.QuickCheck
import Data.Int

import Control.DeepSeq
import Data.List

-------------------------------------------------------------------------------
-- Question 1: Checksum
-------------------------------------------------------------------------------

-- Check that student solutions returns False for empty list.
prop_checksum_empty :: Property
prop_checksum_empty = Student.checksum ([] :: [Int]) ~= False

-- Check that student returns False checksum if list length is not 8.
prop_checksum_length :: [Int] -> Property
prop_checksum_length ns =
  (length ns /= 8) ==> Student.checksum ns ~= False

-- Check that student returns False checksum if sum of digits not divisible by 11.
prop_checksum_digits :: [Int] -> Property
prop_checksum_digits ns =
  not (sum ns `mod` 11 == 0) ==> Student.checksum ns ~= False

-- Check that student returns True checksum for input with True checksum conditions
prop_checksum_correct :: [Int] -> Property
prop_checksum_correct ns = Student.checksum ns ~= True

-------------------------------------------------------------------------------
-- Question 2: Golf Scores
-------------------------------------------------------------------------------

-- Test that student gives 5 points for a "Hole-in-one"
prop_golfScorer_hole :: Integer -> Property
prop_golfScorer_hole par =
  Student.golfScorer par 1 ~= 5

-- Test that student gives 4 points for an "Eagle"
prop_golfScorer_eagle :: Integer -> Integer -> Property
prop_golfScorer_eagle par strokes =
  ((par - strokes) >= 2) && (strokes /= 1) ==>
  Student.golfScorer par strokes ~= 4

-- Test that student gives 3 points for a "Birdie"
prop_golfScorer_birdie :: Integer -> Integer -> Property
prop_golfScorer_birdie par strokes =
  ((par - strokes) == 1) && (strokes /= 1) ==>
  Student.golfScorer par strokes ~= 3

-- Test that student gives 2 points for a "Par"
prop_golfScorer_par :: Integer -> Integer -> Property
prop_golfScorer_par par strokes =
  ((par - strokes) == 0) && (strokes /= 1) ==>
  Student.golfScorer par strokes ~= 2

-- Test that student gives 1 point for a "Bogey"
prop_golfScorer_bogey :: Integer -> Integer -> Property
prop_golfScorer_bogey par strokes =
  ((par - strokes) == -1) && (strokes /= 1) ==>
  Student.golfScorer par strokes ~= 1

-- Test that student gives 0 points otherwise
prop_golfScorer_zero :: Integer -> Integer -> Property
prop_golfScorer_zero par strokes =
  ((par - strokes) < -1) && (strokes /= 1) ==> Student.golfScorer par strokes ~= 0

-------------------------------------------------------------------------------
-- Question 3: List Comprehensions
-------------------------------------------------------------------------------

-- Part a

-- Test if student solution is correct up to first 25 integers
prop_highlyDivisible_correct_15 :: Property
prop_highlyDivisible_correct_15 =
  p_permOf (Student.highlyDivisible 15) (Solutions.highlyDivisible 15)

-- Test if student solution is correct up to first 10 integers
prop_highlyDivisible_correct_10 :: Property
prop_highlyDivisible_correct_10 =
  p_permOf (Student.highlyDivisible 10) (Solutions.highlyDivisible 10)

--Custom test property to check if any element of list satisfies question
highlyDivisible_correct_1 :: [Int] -> Bool
highlyDivisible_correct_1 []       = False
highlyDivisible_correct_1 (x : xs) =
  all (\ k -> x `mod` k == 0) [2..12] || highlyDivisible_correct_1 xs

--Test if any of the first 5 elements of students list is divisible by [1..12]
prop_highlyDivisible_correct_1 :: Property
prop_highlyDivisible_correct_1 =
  counterexample
    ("Your output " ++ show s
     ++ " up to n = 5 does contain any numbers which are divisible by [1..12]")
    (highlyDivisible_correct_1 s)
 where
  s = deepseq (Student.highlyDivisible 5) (Student.highlyDivisible 5)

-- Part b

-- Test if student solution is correct up to first 100 integers.
prop_largestOddFactor_100 :: Property
prop_largestOddFactor_100 =
  Student.largestOddFactor 100 ~= Solutions.largestOddFactor 100

--------------------------------------------------------------------------------
-- Question 4
--------------------------------------------------------------------------------

g1 :: Int8 -> Int8
g1 x = if x == -116 then 0 else x + 1

g2 :: Int8 -> Int8
g2 x = if x == -116 then 1 else x + 1

g3 :: Int8 -> Int8
g3 x = if x == -116 then -1 else x + 1

g4 :: Int8 -> Int8
g4 x = if x == -116 then x + 116 else x + 1

prop_fun1 :: Property
prop_fun1 = Student.equals g1 g2 ~= False

prop_fun2 :: Property
prop_fun2 = Student.equals g2 g3 ~= False

prop_fun3 :: Property
prop_fun3 = Student.equals g1 g4 ~= True

--------------------------------------------------------------------------------
-- Question 5
--------------------------------------------------------------------------------

backToDecimal :: [Integer] -> Integer
backToDecimal ns = sum . map g $ zip [0..l] (reverse ns)
  where
    l :: Integer
    l = fromIntegral (length ns)

    g :: (Integer, Integer) -> Integer
    g (i, n) = (60 ^ i) * n

toBase60 :: Integer -> [Integer]
toBase60 n = g n []
  where
    g :: Integer -> [Integer] -> [Integer]
    g n ds | n == 0    = ds
    g n ds | otherwise = g q (r:ds)
                           where
                             m = n - r
                             r = n `mod` 60
                             q = m `div` 60

prop_retract :: Property
prop_retract =
  forAll (chooseInteger (0, 788)) (\n -> backToDecimal (toBase60 n) ~= n)

smallInteger :: Gen Integer
smallInteger = chooseInteger (0, 6)

arbitraryBase60 :: Gen [Integer]
arbitraryBase60 = do
  k  <- chooseInt (0, 7)
  ns <- vectorOf k (chooseInteger (0, 15))
  return $ dropWhile (== 0) ns

prop_section :: Property
prop_section =
  forAll arbitraryBase60 (\ns -> toBase60 (backToDecimal ns) == ns)

prop_babylonianCorrect :: Property
prop_babylonianCorrect =
  let
    stu = take 25 $ Student.babylonianPalindromes
    sol = take 25 $ Solutions.babylonianPalindromes
  in
    p_permOf stu sol

-- Partial.
prop_babylonianCorrectExceptFirst60 :: Property
prop_babylonianCorrectExceptFirst60 =
  let
    stu = take 25 . filter (> 60) $ Student.babylonianPalindromes
    sol = take 25 $ Solutions.babylonianPalindromes
  in
    p_permOf stu sol
