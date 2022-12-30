module Homework7Marking where

import Types
import MarkingCore

import Test.QuickCheck

import qualified Homework7Solutions as Solutions
import Homework7TestCases

main :: IO ()
main = runMarking tests True
  where
    tests = [ test_phoneToString,
              test_phoneToString_section,
              test_stringToPhone,
              test_stringToPhone_section,
              test_fingerTaps
            ]

-- arbitrary helpers for generating Text
arbitraryChar :: Gen Char
arbitraryChar = elements $ ['a'..'z']++['A'..'Z']
                         ++['0'..'9']++['.',',',' ']

shrinkChar :: Char -> [Char]
shrinkChar _ = []

arbitraryText :: Gen Text
arbitraryText = listOf1 arbitraryChar

shrinkText :: Text -> [Text]
shrinkText t = [take n t | n <- [0..length t - 1]]

-- arbitrary helpers for generating phone input, i.e. [(Button,Presses)]
arbitraryPhone :: Gen [(Button,Presses)]
arbitraryPhone = Solutions.stringToPhone <$> arbitraryText

shrinkPhone :: [(Button,Presses)] -> [[(Button,Presses)]]
shrinkPhone bps = map Solutions.stringToPhone (shrinkText t)
  where
    t = Solutions.phoneToString bps

-- phoneToString tests
test_phoneToString = Test {
  mark        = 15,
  description = newSection ++ "Checking that 'phoneToString' correct...",
  successMsg  = "You got 15 marks because 'phoneToString' is correct.",
  failMsg     = "'phoneToString' did not work correctly.",
  prop        = makeUnaryPropWith prop_phoneToString arbitraryPhone shrinkPhone,
  condition   = Always
  }

test_phoneToString_section = Test {
  mark        = 10,
  description = "Checking that 'stringToPhone (phoneToString bps) == bps'...",
  successMsg = "You got 10 marks because 'stringToPhone' after 'phoneToString' is\
               \ the identity.",
  failMsg     = "'stringToPhone (phoneToString bps)' did not always return\
                \ 'bps'",
  prop        = makeUnaryPropWith prop_phoneToString_section
                arbitraryPhone shrinkPhone,
  condition   = IfFail test_phoneToString
  }

-- stringToPhone tests
test_stringToPhone = Test {
  mark        = 15,
  description = newSection ++ "Checking that 'stringToPhone' correct...",
  successMsg  = "You got 15 marks because 'stringToPhone' is correct.",
  failMsg     = "'stringToPhone' did not work correctly.",
  prop        = makeUnaryPropWith prop_stringToPhone arbitraryText shrinkText,
  condition   = Always
  }

test_stringToPhone_section = Test {
  mark        = 10,
  description = "Checking that 'phoneToString (stringToPhone t) == t'...",
  successMsg = "You got 10 marks because 'phoneToString' after 'stringToPhone'\
               \ is the identity.",
  failMsg     = "'phoneToString (stringToPhone t)' did not always return 't'",
  prop        = makeUnaryPropWith prop_stringToPhone_section
                arbitraryText shrinkText,
  condition   = IfFail test_stringToPhone
  }

-- fingerTaps test
test_fingerTaps = Test {
  mark        = 20,
  description = newSection ++ "Checking that 'fingerTaps' is correct...",
  successMsg  = "You got 20 marks because 'fingerTaps' is correct.",
  failMsg     = "'fingerTaps' did not work correctly.",
  prop        = makeUnaryPropWith prop_fingerTaps arbitraryText shrinkText,
  condition   = Always
  }
