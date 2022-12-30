{-# LANGUAGE StandaloneDeriving #-}

module Homework3TestCases where

import MarkingCore

import Types
import TestCasesUtils
import qualified Homework3 as Student
import qualified Homework3Solutions as Solutions

import Test.QuickCheck

import Control.DeepSeq
import Data.Char
import Data.List

instance NFData Classification where
  rnf _ = ()

-------------------------------------------------------------------------------
-- Question 1: gasUsage
-------------------------------------------------------------------------------

-- Check the 0 case to award partial marks.
prop_gasUsage_0 :: Property
prop_gasUsage_0 = Student.gasUsage 0 ~= Low

-- The gas < 3 case.
prop_gasUsage_below_3 :: Float -> Property
prop_gasUsage_below_3 x =
  x < 3 ==> Student.gasUsage x ~= Low

-- The edge case of gas = 3.
prop_gasUsage_3 :: Property
prop_gasUsage_3 = Student.gasUsage 3 ~= Medium

-- The case of gas being strictly between 3 and 5.
prop_gasUsage_between_3_and_5 :: Float -> Property
prop_gasUsage_between_3_and_5 x =
  3 < x && x < 5 ==> Student.gasUsage x ~= Medium

-- The edge case of exactly 5.
prop_gasUsage_5 :: Property
prop_gasUsage_5 = Student.gasUsage 5 ~= High

prop_gasUsage_between_5_and_7 :: Float -> Property
prop_gasUsage_between_5_and_7 x =
  5 < x && x < 7 ==> Student.gasUsage x ~= High

prop_gasUsage_7 :: Property
prop_gasUsage_7 = Student.gasUsage 7 ~= SuperHigh

prop_gasUsage_above_7 :: Float -> Property
prop_gasUsage_above_7 x = x > 7 ==> Student.gasUsage x ~= SuperHigh

-------------------------------------------------------------------------------
-- Question 2: Luhn's algorithm
-------------------------------------------------------------------------------

-- Test cases from FuncProg2020/fp-learning/Assignments/Formative1/Formative1TestCases.hs

-- Test that student solution is correct by comparison with model solution
prop_luhnDouble_correct :: Int -> Property
prop_luhnDouble_correct d = Student.luhnDouble d ~= Solutions.luhnDouble d

-- Test if student returns single digit output on singe digit input.
prop_luhnDouble_digit :: Int -> Property
prop_luhnDouble_digit d =
-- We leave the precondition for clarity, even though we use a custom generator
-- (to avoid too many discarded tests).
  d `elem` [0..9] ==>
    counterexample ("Your output " ++ show s ++
                     " is not a digit, while its input " ++ show d ++ " was.")
      (s `elem` [0..9])
      where
        s = Student.luhnDouble d

prop_luhnDouble_double :: Int -> Property
prop_luhnDouble_double d =
-- We leave the precondition for clarity, even though we use a custom generator
-- (to avoid too many discarded tests).
  d `elem` [0..9] ==>
    counterexample ("Your output " ++ show s ++
                    " is not less than the input " ++ show d ++ " doubled.")
      (s <= 2 * d)
      where
        s = Student.luhnDouble d

prop_luhn_positive :: Int -> Int -> Int -> Int -> Property
-- This test is used with a custom generator.
prop_luhn_positive a b c d = Student.luhn a b c d ~= True

prop_luhn_negative :: Int -> Int -> Int -> Int -> Property
-- This test is used with a custom generator.
prop_luhn_negative a b c d = Student.luhn a b c d ~= False
