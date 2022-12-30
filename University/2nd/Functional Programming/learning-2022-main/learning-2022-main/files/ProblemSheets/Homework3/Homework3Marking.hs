module Homework3Marking where

import MarkingCore
import Homework3TestCases
import qualified Homework3 as Student

import Test.QuickCheck
import Control.Monad

main :: IO ()
main = runMarking tests True
  where
    tests = [ test_gasUsage0              -- 3  marks
            , test_gasUsageBelow3         -- 4  marks
            , test_gasUsage3              -- 3  marks
            , test_gasUsageBetween3And5   -- 3  marks
            , test_gasUsage5              -- 3  marks
            , test_gasUsageBetween5And7   -- 3  marks
            , test_gasUsage7              -- 3  marks
            , test_gasUsageAbove7         -- 3  marks
            , test_luhnDouble_correct     -- 10 marks
            , test_luhnDouble_digit       -- 2  marks
            , test_luhnDouble_double      -- 2  marks
            , test_luhn_positive          -- 7  marks
            , test_luhn_negative          -- 8  marks
            ]


-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

-- Question 1

openInterval :: Float -> Float -> Gen Float
openInterval x y = arbitrary `suchThat` (\n -> x < n && n < y)

-- Question 2

type Digit = Int

arbitraryDigit :: Gen Digit
arbitraryDigit = elements [0..9]

shrinkDigit :: Digit -> [Digit]
shrinkDigit = return

type BankCardNumber = (Int,Int,Int,Int)

arbitraryBankCardNumber :: Gen BankCardNumber
arbitraryBankCardNumber =
  suchThat (liftM4 (,,,) arbitraryDigit arbitraryDigit arbitraryDigit arbitraryDigit)
  (\(a, b, c, d) ->
     (Student.luhnDouble a + b + Student.luhnDouble c + d) `mod` 10 == 0)

arbitraryInvalidBankCardNumber :: Gen BankCardNumber
arbitraryInvalidBankCardNumber =
  suchThat (liftM4 (,,,) arbitraryDigit arbitraryDigit arbitraryDigit arbitraryDigit)
  (\(a, b, c, d) ->
     (Student.luhnDouble a + b + Student.luhnDouble c + d) `mod` 10 /= 0)

shrinkBankCardNumber :: BankCardNumber -> [BankCardNumber]
shrinkBankCardNumber = return

-------------------------------------------------------------------------------
-- Tests: Question 1: gasUsage
-------------------------------------------------------------------------------

test_gasUsage0 :: Test
test_gasUsage0 = Test {
  mark        = 3,
  description = newSection
                ++ "Checking if the 'gasUsage' function works on input 0...",
  successMsg  = "You got 3 marks for correctly computing the gas usage\
                 \ classification on 0.",
  failMsg     = "Your implementation of 'gasUsage' computed an incorrect\
                 \ classification on 0.",
  prop        = makeNullaryProp prop_gasUsage_0,
  condition   = Always
}

test_gasUsageBelow3 :: Test
test_gasUsageBelow3 = Test {
  mark        = 4,
  description = "Checking if the 'gasUsage' function works with numbers below\
                 \ 3...",
  successMsg  = "You got 4 marks for correctly computing the gas usage\
                 \ classification for numbers below 3.",
  failMsg     = "Your implementation of 'gasUsage' computed an incorrect\
                 \ classification for a number below 3.",
  prop        = makeUnaryPropWith
                  prop_gasUsage_below_3
                  (openInterval 0 3)
                  return,
  condition   = Always
}

test_gasUsage3 :: Test
test_gasUsage3 = Test {
  mark        = 3,
  description = "Checking if the 'gasUsage' function works on input 3...",
  successMsg  = "You got 3 marks for correctly computing the gas usage\
                 \ classification for input 3.",
  failMsg     = "Your implementation of 'gasUsage' computed an incorrect\
                 \ classification for input 3.",
  prop        = makeNullaryProp prop_gasUsage_3,
  condition   = Always
}

test_gasUsageBetween3And5 :: Test
test_gasUsageBetween3And5 = Test {
  mark        = 3,
  description = "Checking if the 'gasUsage' function works with numbers\
                \ (strictly) in between 3 and 5...",
  successMsg  = "You got 3 marks for correctly computing the gas usage\
                 \ classification for numbers (strictly) in between 3 and 5.",
  failMsg     = "Your implementation of 'gasUsage' computed an incorrect\
                 \ classification for a value (strictly) in between 3 and 5.",
  prop        = makeUnaryPropWith
                  prop_gasUsage_between_3_and_5
                  (openInterval 3 5)
                  return,
  condition   = Always
}

test_gasUsage5 :: Test
test_gasUsage5 = Test {
  mark        = 3,
  description = "Checking if the 'gasUsage' function works on input 5...",
  successMsg  = "You got 3 marks for correctly computing the gas usage\
                 \ classification for input 5.",
  failMsg     = "Your implementation of 'gasUsage' computed an incorrect\
                 \ classification for input 5.",
  prop        = makeNullaryProp prop_gasUsage_5,
  condition   = Always
}

test_gasUsageBetween5And7 :: Test
test_gasUsageBetween5And7 = Test {
  mark        = 3,
  description = "Checking if the 'gasUsage' function works with numbers\
                \ (strictly) in between 5 and 7...",
  successMsg  = "You got 3 marks for correctly computing the gas usage\
                 \ classification for numbers (strictly) in between 5 and 7.",
  failMsg     = "Your implementation of 'gasUsage' computed an incorrect\
                 \ classification for a value (strictly) in between 5 and 7.",
  prop        = makeUnaryPropWith
                  prop_gasUsage_between_5_and_7
                  (openInterval 5 7)
                  return,
  condition   = Always
}

test_gasUsage7 :: Test
test_gasUsage7 = Test {
  mark        = 3,
  description = "Checking if the 'gasUsage' function works on input 7...",
  successMsg  = "You got 3 marks for correctly computing the gas usage\
                 \ classification for input 7.",
  failMsg     = "Your implementation of 'gasUsage' computed an incorrect\
                 \ classification for input 7.",
  prop        = makeNullaryProp prop_gasUsage_7,
  condition   = Always
}

test_gasUsageAbove7 :: Test
test_gasUsageAbove7 = Test {
  mark        = 3,
  description = "Checking if the 'gasUsage' function works on numbers above 7...",
  successMsg  = "You got 3 marks for correctly computing the gas usage\
                 \ for numbers above 7.",
  failMsg     = "Your implementation of 'gasUsage' computed an incorrect\
                 \ classification for a number above 7.",
  prop        = makeUnaryPropWith
                  prop_gasUsage_above_7
                  (openInterval 7 100)
                  return,
  condition   = Always
}

-------------------------------------------------------------------------------
-- Tests: Question 2: Luhn's algorithm
-------------------------------------------------------------------------------

test_luhnDouble_correct = Test {
  mark = 10,
  description = newSection ++ "Checking if 'luhnDouble' is correct...",
  successMsg = "You got 10 marks for correctly implementing 'luhnDouble'.",
  failMsg = "Your 'luhnDouble' was not quite correct.",
  prop = makeUnaryPropWith prop_luhnDouble_correct arbitraryDigit shrinkDigit,
  condition = Always
}

test_luhnDouble_digit = Test {
  mark = 2,
  description = "Checking if 'luhnDouble' returns digits (with digits as input)...",
  successMsg = "You got 2 marks for having 'luhnDouble' return a digit (when its input is also a digit).",
  failMsg = "Your 'luhnDouble' incorrectly did not return a digit, even though its input was.",
  prop = makeUnaryPropWith prop_luhnDouble_digit arbitraryDigit shrinkDigit,
  condition = IfFail test_luhnDouble_correct
}

test_luhnDouble_double = Test {
  mark = 2,
  description = "Checking if the output of 'luhnDouble' is less than the input doubled...",
  successMsg = "You got 2 marks for having 'luhnDouble' return a number less than its input doubled.",
  failMsg = "Your 'luhnDouble' incorrectly returned a number that is greater than the input doubled.",
  prop = makeUnaryPropWith prop_luhnDouble_double arbitraryDigit shrinkDigit,
  condition = IfFail test_luhnDouble_correct
}

test_luhn_positive = Test {
  mark = 7,
  description = "Checking if 'luhn' correctly identifies valid bank card numbers...",
  successMsg = "You got 7 marks for correctly identifying valid bank card numbers.",
  failMsg = "Some valid bank card numbers were incorrectly identified as invalid."  ,
  prop = makeQuarternaryPropWith prop_luhn_positive arbitraryBankCardNumber shrinkBankCardNumber,
  condition = Always
}

test_luhn_negative = Test {
  mark = 8,
  description = "Checking if 'luhn' correctly identifies invalid bank card numbers...",
  successMsg = "You got 8 marks for correctly identifying invalid bank card numbers.",
  failMsg = "Some invalid bank card numbers were incorrectly identified as valid."  ,
  prop = makeQuarternaryPropWith prop_luhn_negative arbitraryInvalidBankCardNumber shrinkBankCardNumber,
  condition = Always
}
