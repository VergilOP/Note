{-# LANGUAGE StandaloneDeriving #-}
module Homework7TestCases where

import Types
import TestCasesUtils
import qualified Homework7 as Student
import qualified Homework7Solutions as Solutions

import Test.QuickCheck

-- stringToPhone tests
prop_phoneToString :: [(Button,Presses)] -> Property
prop_phoneToString bps =
  Student.phoneToString bps ~= Solutions.phoneToString bps

prop_phoneToString_section :: [(Button,Presses)] -> Property
prop_phoneToString_section bps =
  Student.stringToPhone (Student.phoneToString bps) ~= bps

-- stringToPhone tests
prop_stringToPhone :: Text -> Property
prop_stringToPhone t = Student.stringToPhone t ~= Solutions.stringToPhone t

prop_stringToPhone_section :: Text -> Property
prop_stringToPhone_section t =
  Student.phoneToString (Student.stringToPhone t) ~= t

-- fingerTaps test
prop_fingerTaps :: Text -> Property
prop_fingerTaps t = Student.fingerTaps t ~= Solutions.fingerTaps t