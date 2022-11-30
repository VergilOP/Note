module PracticeTestMarking where

import MarkingCore
import PracticeTestTestCases
import qualified PracticeTest as Student

import Test.QuickCheck
import Control.Monad

main :: IO ()
main = runMarking tests True
  where
    tests = [ test_checksum_empty                       -- 2  marks
            , test_checksum_length                      -- 2  marks
            , test_checksum_digits                      -- 3  marks
            , test_checksum_correct                     -- 3  marks
            , test_golfScorer_hole                      -- 1  mark
            , test_golfScorer_eagle                     -- 2  marks
            , test_golfScorer_birdie                    -- 2  marks
            , test_golfScorer_par                       -- 2  marks
            , test_golfScorer_bogey                     -- 2  marks
            , test_golfScorer_zero                      -- 1  mark
            , test_highlyDivisible_correct_15           -- 5  marks
            , test_highlyDivisible_correct_10           -- 3  marks
                                                        --    (only if correct_20
                                                        --    fails)
            , test_highlyDivisible_correct_1            -- 1  mark
                                                        --    (only if correct_10
                                                        --    fails)
            , test_largestOddFactor_100                 -- 5  marks
            , test_fun1                                 -- 4  marks
            , test_fun2                                 -- 3  marks
            , test_fun3                                 -- 3  marks
            , test_babylonianCorrectness                -- 10 marks
            , test_babylonianCorrectnessExceptFirst60   -- 7  marks
            ]


-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

-- Question 1

arbitraryDigit :: Gen Int
arbitraryDigit = elements [0..9]

arbitraryDigitsNotOfLength8 :: Gen [Int]
arbitraryDigitsNotOfLength8 = do
  n <- elements $ [1..7] ++ [9..20]
  vectorOf n arbitraryDigit

arbitrary8Digits :: Gen [Int]
arbitrary8Digits = vectorOf 8 arbitraryDigit

arbitraryValidDigits :: Gen [Int]
arbitraryValidDigits =
  arbitrary8Digits `suchThat` (\ds -> sum ds `rem` 11 == 0)

-- Question 2

arbitraryGolfScoresHole :: Gen Integer
arbitraryGolfScoresHole = elements [1..12]

arbitraryGolfScoresEagle :: Gen (Integer,Integer)
arbitraryGolfScoresEagle = elements [(p,s) | p <- [1..12] , s <- [1..12]
                                           , (p-s) >= 2 , s /= 1]

arbitraryGolfScoresBirdie :: Gen (Integer,Integer)
arbitraryGolfScoresBirdie = elements [(p,s) | p <- [1..12] , s <- [1..12]
                                            , (p-s) == 1 , s /= 1]

arbitraryGolfScoresPar :: Gen (Integer,Integer)
arbitraryGolfScoresPar = elements [(p,s) | p <- [1..12] , s <- [1..12]
                                         , (p-s) == 0 , s /= 1]

arbitraryGolfScoresBogey :: Gen (Integer,Integer)
arbitraryGolfScoresBogey = elements [(p,s) | p <- [1..12] , s <- [1..12]
                                           , (p-s) == -1 , s /= 1]

arbitraryGolfScoresZero :: Gen (Integer,Integer)
arbitraryGolfScoresZero = elements [(p,s) | p <- [1..12] , s <- [1..12]
                                          , (p-s) < -1 , s /= 1]

-------------------------------------------------------------------------------
-- Tests: Question 1: Checksum
-------------------------------------------------------------------------------

test_checksum_empty = Test
  { mark        = 2
  , description = newSection ++ "Checking that 'checksum []' returns False..."
  , successMsg  = "You got 2 marks because your checksum function returned\
                   \ False on []."
  , failMsg     = "Your checksum function did not work correctly on the input []."
  , prop        = makeNullaryProp prop_checksum_empty
  , condition   = Always
  }

test_checksum_length = Test
  { mark        = 2
  , description = "Checking that 'checksum' returns False on lists that are not\
                   \ of length 8..."
  , successMsg  = "You got 2 marks because your 'checksum' function returned\
                   \ False on lists that are not of length 8."
  , failMsg     = "Your 'checksum' function did not work correctly on a list of digits\
                   \ that is not of length 8."
  , prop        = makeUnaryPropWith prop_checksum_length arbitraryDigitsNotOfLength8 (const [])
  , condition   = Always
  }

test_checksum_digits = Test
  { mark        = 3
  , description = "Checking that 'checksum' returns False on lists of digits\
                   \ of length 8 that are not divisible by 11..."
  , successMsg  = "You got 3 marks because your 'checksum' function returned\
                   \ False on lists of digits of length 8 and with a sum not\
                   \ divisible by 11."
  , failMsg     = "Your 'checksum' did not work correctly on a list of digits\
                   \ of length 8 whose sum is not divisible by 11."
  , prop        = makeUnaryPropWith prop_checksum_digits arbitrary8Digits (const [])
  , condition   = Always
  }

test_checksum_correct = Test
  { mark        = 3
  , description = "Checking that 'checksum' returns True for lists of digits\
                   \ meeting the validity criteria."
  , successMsg  = "You got 3 marks because your 'checksum' function returned True\
                   \ on lists of digits meeting the validity criteria (namely,\
                   \ being of length 8 and being divisible by 11)"
  , failMsg     = "Your 'checksum' function did not work correctly on a list of digits\
                   \ meeting the validity criteria."
  , prop        = makeUnaryPropWith prop_checksum_correct arbitraryValidDigits (const [])
  , condition   = Always
  }

-------------------------------------------------------------------------------
-- Tests: Question 2: Golf Scores
-------------------------------------------------------------------------------

test_golfScorer_hole = Test {
  mark = 1,
  description = newSection ++ "Checking that 'golfScorer' correctly gives 5 points for a 'Hole-in-one'...",
  successMsg = "You got 1 mark because 'golfScorer' correctly gives 5 points for a 'Hole-in-one'.",
  failMsg = "'golfScorer' did not give 5 points for a 'Hole-in-one' (when strokes == 1).",
  prop = makeUnaryPropWith prop_golfScorer_hole arbitraryGolfScoresHole return,
  condition = Always
}

test_golfScorer_eagle = Test {
  mark = 2,
  description = "Checking that 'golfScorer' correctly gives 4 points for an 'Eagle'...",
  successMsg = "You got 2 marks because 'golfScorer' correctly gives 4 points for an 'Eagle'.",
  failMsg = "'golfScorer' did not give 4 points for an 'Eagle' (when par - strokes >= 2 and strokes != 1).",
  prop = makeBinaryPropWith prop_golfScorer_eagle arbitraryGolfScoresEagle return,
  condition = Always
}

test_golfScorer_birdie = Test {
  mark = 2,
  description = "Checking that 'golfScorer' correctly gives 3 points for a 'Birdie'...",
  successMsg = "You got 2 marks because 'golfScorer' correctly gives 3 points for a 'Birdie'.",
  failMsg = "'golfScorer' did not give 3 points for a 'Birdie' (when par - strokes == 1 and strokes != 1).",
  prop = makeBinaryPropWith prop_golfScorer_birdie arbitraryGolfScoresBirdie return,
  condition = Always
}

test_golfScorer_par = Test {
  mark = 2,
  description = "Checking that 'golfScorer' correctly gives 2 points for a 'Par'...",
  successMsg = "You got 2 marks because 'golfScorer' correctly gives 2 points for a 'Par'.",
  failMsg = "'golfScorer' did not give 2 points for a 'Par' (when par == strokes and strokes != 1).",
  prop = makeBinaryPropWith prop_golfScorer_par arbitraryGolfScoresPar return,
  condition = Always
}

test_golfScorer_bogey = Test {
  mark = 2,
  description = "Checking that 'golfScorer' correctly gives 1 point for a 'Bogey'...",
  successMsg = "You got 2 marks because 'golfScorer' correctly gives 1 point for a 'Bogey'.",
  failMsg = "'golfScorer' did not give 1 point for a 'Bogey' (when par - strokes == -1 and strokes != 1).",
  prop = makeBinaryPropWith prop_golfScorer_bogey arbitraryGolfScoresBogey return,
  condition = Always
}

test_golfScorer_zero = Test {
  mark = 1,
  description = "Checking that 'golfScorer' correctly gives 0 points for worse than a 'Bogey'...",
  successMsg = "You got 1 mark because 'golfScorer' correctly gives 0 points for worse than a 'Bogey'.",
  failMsg = "'golfScorer' did not give 0 points for worse than a 'Bogey' (when par - strokes < -1 and strokes != 1).",
  prop = makeBinaryPropWith prop_golfScorer_zero arbitraryGolfScoresZero return,
  condition = Always
}

-------------------------------------------------------------------------------
-- Tests: Question 3: ListComprehensions
-------------------------------------------------------------------------------

---------
-- Part a
---------

test_highlyDivisible_correct_15 :: Test
test_highlyDivisible_correct_15 = Test {
  mark        = 5,
  description = newSection
                ++ "Checking if 'highlyDivisible' list with n = 15 is permutation of correct answer...",
  successMsg  = "You got 5 marks for correctly implementing 'highlyDivisible'.",
  failMsg     = "Your 'highlyDivisible' was not quite correct for n = 15.",
  prop        = makeNullaryProp prop_highlyDivisible_correct_15,
  condition   = Always
}

test_highlyDivisible_correct_10 :: Test
test_highlyDivisible_correct_10 = Test {
  mark        = 3,
  description = "Checking if 'highlyDivisible' list with n = 10 is permutation\
                \ of correct answer...",
  successMsg  = "You got 3 marks for correctly implementing 'highlyDivisible' up to length 10.",
  failMsg     = "Your 'highlyDivisible' was not quite correct for n = 10.",
  prop        = makeNullaryProp prop_highlyDivisible_correct_10,
  condition   = IfFail test_highlyDivisible_correct_15
}

test_highlyDivisible_correct_1 :: Test
test_highlyDivisible_correct_1 = Test {
  mark        = 1,
  description = "Checking if 'highlyDivisible' list contains some correct element in first 5...",
  successMsg  = "You got 1 mark for giving some correct answer in the first 5 elements of your list.",
  failMsg     = "Your 'highlyDivisible' is not correct.",
  prop        = makeNullaryProp prop_highlyDivisible_correct_1,
  condition   = IfFail test_highlyDivisible_correct_10
}

---------
-- Part b
---------

test_largestOddFactor_100 :: Test
test_largestOddFactor_100 = Test {
  mark        = 5,
  description = "Checking if 'largestOddFactor' list with n = 100 is correct...",
  successMsg  = "You got 5 marks for correctly implementing largestOddFactor.",
  failMsg     = "Your 'largestOddFactor' was not quite correct for n = 100.",
  prop        = makeNullaryProp prop_largestOddFactor_100,
  condition   = Always
}


--------------------------------------------------------------------------------
-- Tests: Question 4: Finite Types
--------------------------------------------------------------------------------

test_fun1 = Test {
  mark        = 4
, description = newSection ++
                "Checking that 'equals' is correct on the functions\n\
                \\tg1 :: Int8 -> Int8\n\
                \\tg1 x = if x == -116 then 0 else x + 1\n\
                \\tg2 :: Int8 -> Int8\n\
                \\tg2 x = if x == -116 then 1 else x + 1\n\
                \which are not equivalent..."
, successMsg  = "You got 4 marks because 'equals' worked correctly on the above\
                \ functions."
, failMsg     = "Your implementation of 'equals' did not work correctly on the\
                \ above functions."
, prop        = makeNullaryProp prop_fun1
, condition   = Always
}

test_fun2 = Test {
  mark        = 3
, description = "Checking that 'equals' is correct on the functions\n\
                \\tg2 :: Int8 -> Int8\n\
                \\tg2 x = if x == -116 then 1 else x + 1\n\
                \\tg3 :: Int8 -> Int8\n\
                \\tg3 x = if x == -116 then -1 else x + 1\n\
                \which are not equivalent..."
, successMsg  = "You got 3 marks because 'equals' is correct on the above\
                \ functions."
, failMsg     = "Your implementation of 'equals' did not work correctly on the\
                \ above functions."
, prop        = makeNullaryProp prop_fun2
, condition   = Always
}

test_fun3 = Test {
    mark      = 3
, description = "Checking that 'equals' is correct on the functions\n\
                \\tg1 :: Int8 -> Int8\n\
                \\tg1 x = if x == -116 then 0 else x + 1\n\
                \\tg4 :: Int8 -> Int8\n\
                \\tg4 x = if x == -116 then x + 116 else x + 1\n\
                \which are equivalent..."
, successMsg  = "You got 3 marks because 'equals' is correct on the above\
                \ functions."
, failMsg     = "Your implementation of 'equals' did not work correctly on the\
                \ above functions."
, prop        = makeNullaryProp prop_fun3
, condition   = Always
}



--------------------------------------------------------------------------------
-- Tests: Question 5: Babylonian Palindromes
--------------------------------------------------------------------------------

test_babylonianCorrectness = Test {
  mark        = 10
, description = newSection ++
                "Checking that your implementation of 'babylonianPalindromes'\
                 \ correctly gives the first 25 Babylonian palindromes..."
, successMsg  = "You got 10 marks because your implementation\
                \ 'babylonianPalindromes' correctly gave the first 25 Babylonian palindromes."
, failMsg     = "Your implementation of 'babylonianPalindromes' did not\
                \ correctly give the first 25 Babylonian palindromes."
, prop        = makeNullaryProp prop_babylonianCorrect
, condition   = Always
}

test_babylonianCorrectnessExceptFirst60 = Test {
  mark        = 7
, description = "Checking that your implementation of 'babylonianPalindromes'\
                 \ correctly gives the first 25 Babylonian palindromes\
                 \ (excluding the single-digit Babylonian palindromes)..."
, successMsg  = "You got 7 marks because your implementation\
                \ 'babylonianPalindromes' correctly gave the first 25 Babylonian\
                \ palindromes (excluding the single-digit Babylonian palindromes)."
, failMsg     = "Your implementation of 'babylonianPalindromes' did not\
                \ correctly give the first 25 Babylonian palindromes,\
                \ even when the single-digit ones were excluded."
, prop        = makeNullaryProp prop_babylonianCorrectExceptFirst60
, condition   = IfFail test_babylonianCorrectness
}
