module ClassTest1Marking where

import MarkingCore
import ClassTest1TestCases
import qualified ClassTest1 as Student

import Test.QuickCheck
import Control.Monad

import Types

main :: IO ()
main = runMarking tests True
  where
    tests = [ test_checkParity_empty              -- 2  marks
            , test_checkParity_length_positive    -- 2  marks
            , test_checkParity_length_negative    -- 2  marks
            , test_checkParity_even               -- 2  marks
            , test_checkParity_odd                -- 2  marks
            , test_substitution_uppercase         -- 2  marks
            , test_substitution_lowercase         -- 2  marks
            , test_substitution_letters           -- 2  marks
            , test_substitution_all               -- 4  marks
            , test_largestPrimeBetween_2          -- 1  mark
            , test_largestPrimeBetween_first100   -- 2  marks
            , test_largestPrimeBetween_next200    -- 2  marks
            , test_strongPrimes_empty             -- 1  mark
            , test_strongPrimes_correct_1         -- 1  mark
            , test_strongPrimes_correct_50        -- 1  mark
            , test_executeCommandsCorrect         -- 10 marks
            , test_executeCommandsSingleMoveLeft  -- |-> 1 mark
            , test_executeCommandsSingleMoveRight -- |-> 1 mark
            , test_executeCommandsSingleMoveUp    -- |-> 1 mark
            , test_executeCommandsSingleMoveDown  -- |-> 1 mark
            , test_atmChangeCorrect               -- 10 marks
            , test_atmChangeSound                 -- |-> 5 marks
            ]


-------------------------------------------------------------------------------
-- Tests: Question 1: Parity
-------------------------------------------------------------------------------

test_checkParity_empty = Test
  { mark        = 2
  , description = newSection ++ "Checking that 'checkParity []' returns True..."
  , successMsg  = "You got 2 marks because your checkParity function returned\
                   \ True on []."
  , failMsg     = "Your checkParity function did not work correctly on the input []."
  , prop        = makeNullaryProp prop_checkParity_empty
  , condition   = Always
  }

test_checkParity_length_positive = Test
  { mark        = 2
  , description = "Checking that 'checkParity' returns True for strings with\
                   \ length divisible by 8..."
  , successMsg  = "You got 2 marks because your checkParity function returned\
                   \ True for strings with length divisible by 8."
  , failMsg     = "Your checkParity function did not work correctly for input\
                   \ strings with length divisible by 8."
  , prop        = makeUnaryPropWith prop_checkParity_length_positive arbitraryEvenBitstring shrinkBitstring
  , condition   = Always
  }

test_checkParity_length_negative = Test
  { mark        = 2
  , description = "Checking that 'checkParity' returns False for strings with\
                   \ length not divisible by 8..."
  , successMsg  = "You got 2 marks because your checkParity function returned\
                   \ False for strings with length not divisible by 8."
  , failMsg     = "Your checkParity function did not reject strings with\
                   \ length not divisible by 8."
  , prop        = makeUnaryPropWith prop_checkParity_length_negative arbitraryBitstringNotOfLengthDivisibleBy8 shrinkBitstring
  , condition   = Always
  }

test_checkParity_even = Test
 { mark        = 2
 , description = "Checking that 'checkParity' returns True for strings where\
                  \ every byte has even parity..."
 , successMsg  = "You got 2 marks because your checkParity function returned\
                  \ True for strings where every byte has even parity."
 , failMsg     = "Your checkParity function rejected a string where every byte\
                  \ has even parity."
 , prop        = makeUnaryPropWith prop_checkParity_even arbitraryEvenBitstring shrinkBitstring
 , condition   = Always
 }

test_checkParity_odd = Test
 { mark        = 2
 , description = "Checking that 'checkParity' returns False for strings where\
                  \ at least one string has odd parity..."
 , successMsg  = "You got 2 marks because your checkParity function returned\
                  \ False for a string where at least one byte has odd parity."
 , failMsg     = "Your checkParity function did not reject a string\
                  \ which contains a byte with odd parity."
 , prop        = makeUnaryPropWith prop_checkParity_odd arbitraryOddBitstring shrinkByteString
 , condition   = Always
 }

-------------------------------------------------------------------------------
-- Tests: Question 2: Substitution
-------------------------------------------------------------------------------

test_substitution_uppercase = Test
  { mark        = 2
  , description = newSection ++ "Checking that 'substitution' is correct for plaintext\
                   \ which consists of only uppercase letters."
  , successMsg  = "You got 2 marks for correctly implementing 'substitution'\
                   \ for plaintext which consists of only uppercase letters"
  , failMsg     = "Your 'substitution' did not correctly encrypt plaintext\
                    \ which consists of only uppercase letters."
  , prop        = makeBinaryPropWith prop_substitution_uppercase arbitraryUppercase shrinkSubstitution
  , condition   = Always
  }

test_substitution_lowercase = Test
  { mark        = 2
  , description = "Checking that 'substitution' is correct for plaintext\
                   \ which consists of only lowercase letters."
  , successMsg  = "You got 2 marks for correctly implementing 'substitution'\
                   \ for plaintext which consists of only lowercase letters"
  , failMsg     = "Your 'substitution' did not correctly encrypt the plaintext\
                    \ which consists of only lowercase letters."
  , prop        = makeBinaryPropWith prop_substitution_lowercase arbitraryLowercase shrinkSubstitution
  , condition   = Always
  }

test_substitution_letters = Test
  { mark        = 2
  , description = "Checking that 'substitution' is correct for plaintext\
                   \ which consists of only letters."
  , successMsg  = "You got 2 marks for correctly implementing 'substitution'\
                   \ for plaintext which consists of only letters"
  , failMsg     = "Your 'substitution' did not correctly encrypt the plaintext\
                    \ which consists of only letters."
  , prop        = makeBinaryPropWith prop_substitution_letters arbitraryLetters shrinkSubstitution
  , condition   = Always
  }

test_substitution_all = Test
  { mark        = 4
  , description = "Checking that 'substitution' is correct for plaintext\
                   \ which consists of letters, spaces and punctuation."
  , successMsg  = "You got 4 marks for correctly implementing 'substitution'\
                   \ for plaintext which consists of letters, spaces and\
                   \ punctuation"
  , failMsg     = "Your 'substitution' did not correctly encrypt the plaintext\
                    \ which consists of letters, spaces and punctuation."
  , prop        = makeBinaryPropWith prop_substitution_all arbitraryPlaintext shrinkSubstitution
  , condition   = Always
  }

-------------------------------------------------------------------------------
-- Tests: Question 3: Primes
-------------------------------------------------------------------------------

---------
-- Part a
---------

test_largestPrimeBetween_2 = Test
  { mark        = 1
  , description = newSection ++ "Checking that 'largestPrimeBetween 2' == 3..."
  , successMsg  = "You got 1 mark because your 'largestPrimeBetween'\
                   \ returned 3 on input 2."
  , failMsg     = "Your 'largestPrimeBetween' did not correctly give\
                   \ the largest prime number for input 2."
  , prop        = makeNullaryProp prop_largestPrimeBetween_2
  , condition   = Always
  }

test_largestPrimeBetween_first100 = Test
  { mark        = 2
  , description = "Checking that 'largestPrimeBetween n' gives the correct\
                   \ largest primes for n <= 102."
  , successMsg  = "You got 2 marks because your 'largestPrimeBetween'\
                   \ gave the first 100 largest primes correctly."
  , failMsg     = "Your 'largestPrimeBetween' did not correctly give\
                   \ the first 100 largest primes."
  , prop        = makeNullaryProp' prop_largestPrimeBetween_first100 10000000
  , condition   = Always
  }

test_largestPrimeBetween_next200 = Test
  { mark        = 2
  , description = "Checking that 'largestPrimeBetween n' gives the correct\
                   \ largest primes for 100 <= n <= 300..."
  , successMsg  = "You got 2 marks because your 'largestPrimeBetween'\
                   \ gives the correct largest primes for 100 <= n <= 300."
  , failMsg     = "Your 'largestPrimeBetween' did not correctly give the\
                   \ correct largest primes for 100 <= n <= 300."
  , prop        = makeNullaryProp' prop_largestPrimeBetween_next200 5000000
  , condition   = Always
  }

---------
-- Part b
---------

test_strongPrimes_empty = Test
  { mark        = 1
  , description = "Checking that 'strongPrimes 0' == []..."
  , successMsg  = "You got 1 mark because your strongPrimes returned []\
                   \ on 0."
  , failMsg     = "Your 'strongPrimes' did not give the empty list for\
                   \ 0."
  , prop        = makeNullaryProp prop_strongPrimes_empty
  , condition   = Always
  }

test_strongPrimes_correct_1 = Test
  { mark        = 1
  , description = "Checking that 'strongPrimes 1' == [11]..."
  , successMsg  = "You got 1 mark because your strongPrimes gave the first\
                   \ strong prime, which is 11."
  , failMsg     = "Your 'strongPrimes' did not give the first strong prime."
  , prop        = makeNullaryProp prop_strongPrimes_correct_1
  , condition   = Always
  }

test_strongPrimes_correct_50 = Test
  { mark        = 3
  , description = "Checking that 'strongPrimes 50' gives the first fifty strong\
                   \ primes..."
  , successMsg  = "You got 3 marks because your strongPrimes gave the first\
                   \ fifty strong primes."
  , failMsg     = "Your 'strongPrimes' did not give the first fifty strong primes."
  , prop        = makeNullaryProp prop_strongPrimes_correct_50
  , condition   = Always
  }

--------------------------------------------------------------------------------
-- Tests: Question 4: Directions
--------------------------------------------------------------------------------

test_executeCommandsCorrect = Test
  { mark        = 10
  , description = newSection
                  ++ "Checking that 'executeCommands' works correctly..."
  , successMsg  = "You got 10 marks because your 'executeCommands'\
                  \ function is correct."
  , failMsg     = "Your 'executeCommands' function did not work correctly."
  , prop        = makeBinaryPropWith
                    prop_executeCommandsCorrect
                    arbitraryCommandListAndCoord
                    shrink
  , condition   = Always
  }

ofTheForm :: Direction -> String
ofTheForm d = "of the form '[(" ++ show d ++ ", n)]'"

singleDirDescription :: Direction -> String
singleDirDescription d = s ++ ofTheForm d ++ "..."
  where
    s = "Checking that 'executeCommands' works correctly on singleton lists "

singleDirSuccessMsg :: Direction -> String
singleDirSuccessMsg d = s1 ++ s2 ++ ofTheForm d ++ "."
  where
    s1 = "You got 2 marks because "
    s2 = "your 'executeCommands' function worked correctly on singleton lists "

singleDirFailMsg :: Direction -> String
singleDirFailMsg d = s1 ++ s2 ++ ofTheForm d ++ "."
  where
    s1 = "Your 'executeCommands' function did not work correctly "
    s2 = "on singleton lists "

makeSingleDirTest :: Direction -> Test
makeSingleDirTest d = Test
  { mark        = 2
  , description = singleDirDescription d
  , successMsg  = singleDirSuccessMsg d
  , failMsg     = singleDirFailMsg d
  , prop        = makeBinaryPropWith
                    (prop_executeCommandsSingleDirCorrect d)
                    arbitraryLengthAndCoord
                    shrink
  , condition   = IfFail test_executeCommandsCorrect
  }

test_executeCommandsSingleMoveLeft  :: Test
test_executeCommandsSingleMoveRight :: Test
test_executeCommandsSingleMoveUp    :: Test
test_executeCommandsSingleMoveDown  :: Test

test_executeCommandsSingleMoveLeft  = makeSingleDirTest MoveLeft
test_executeCommandsSingleMoveRight = makeSingleDirTest MoveRight
test_executeCommandsSingleMoveUp    = makeSingleDirTest MoveUp
test_executeCommandsSingleMoveDown  = makeSingleDirTest MoveDown

--------------------------------------------------------------------------------
-- Tests: Question 5: ATM
--------------------------------------------------------------------------------

test_atmChangeCorrect = Test
  { mark        = 10
  , description = newSection ++ desc
  , successMsg  = succMsg
  , failMsg     = failMsg
  , prop        = makeBinaryPropWith
                    prop_atmChangeCorrect
                    arbitraryDenominationListAndAmount'
                    (const [])
  , condition   = Always
  }
    where
      desc    = "Checking that your 'atmChange' function works correctly..."
      succMsg = "You got 10 marks because your 'atmChange' function worked\
                \ correctly."
      failMsg = "Your 'atmChange' function did not work correctly."

test_atmChangeSound = Test
  { mark        = 5
  , description = desc
  , successMsg  = succMsg
  , failMsg     = failMsg
  , prop        = makeBinaryPropWith
                    prop_atmChangeSound
                    arbitraryDenominationListAndAmount'
                    (const [])
  , condition   = IfFail test_atmChangeCorrect
  }
    where
      desc    = "Checking that the sum expressed by the list returned by your\
                \ 'atmChange' function corresponds to the given amount..."
      succMsg = "You got 5 marks because your 'atmChange' returned lists expressing\
                \ a sum corresponding to the given amount."
      failMsg = "Your 'atmChange' returned an incorrect list."
