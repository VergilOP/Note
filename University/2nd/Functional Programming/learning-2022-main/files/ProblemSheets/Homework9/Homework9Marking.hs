module Homework9Marking where

import Types
import MarkingCore

import Test.QuickCheck

import qualified Homework9Solutions as Solutions
import Homework9TestCases

main :: IO ()
main = runMarking tests True
  where
    tests = [ test_fac_1
            , test_fac_25
            , test_fac_50
            , test_applyfunsLeaf
            , test_applyfunsFork
            , test_updateNodesEmpty
            , test_updateNodesNode
            , test_updateNodesFull
            , test_updateNodesPartial
            , test_evalAll
            , test_evalDivFree
            , test_runAll
            , test_runDivFree
            ]

--------------------------------------------------------------------------------
-- EXERCISE 0
--------------------------------------------------------------------------------

test_fac_1 :: Test
test_fac_1 = Test {
  mark        = 2,
  description = "Testing that `factorial 1` returns 1.",
  successMsg  = "You got 2 marks because `factorial 1` returned the correct\
                \ solution.",
  failMsg     = "Your `factorial 1` did not return the correct solution.",
  prop        = makeNullaryProp prop_fac_1,
  condition   = Always
}

test_fac_25 :: Test
test_fac_25 = Test {
  mark        = 4,
  description = "Testing that `factorial 25` returns the correct solution.",
  successMsg  = "You got 4 marks because `factorial 25` returned the correct\
                 \solution.",
  failMsg     = "Your `factorial 25` did not return the correct solution.",
  prop        = makeNullaryProp prop_fac_25,
  condition   = Always
}

test_fac_50 :: Test
test_fac_50 = Test {
  mark        = 4,
  description = "Testing that `factorial 50` returns the correct solution.",
  successMsg  = "You got 4 marks because `factorial 50` returned the correct\
                 \solution.",
  failMsg     = "Your `factorial 50` did not return the correct solution.",
  prop        = makeNullaryProp prop_fac_50,
  condition   = Always
}

--------------------------------------------------------------------------------
-- EXERCISE 1
--------------------------------------------------------------------------------

test_applyfunsLeaf = Test
  { mark        = 5
  , description = newSection ++ "Checking that your 'applyfuns' is correct when asked\n\
                  \to apply 'length' and 'sum' onto a single Leaf..."
  , successMsg  = "You got 5 marks because your 'applyfuns length sum (Leaf ns)'\
                  \ returned 'Leaf (sum ns)'."
  , failMsg     = "Your 'applyfuns length sum (Leaf ns)' did not return 'Leaf\
                  \ (sum ns)'."
  , prop        = makeUnaryProp prop_applyfunsLeaf
  , condition   = Always
  }

test_applyfunsFork = Test
  { mark        = 5
  , description = "Checking that your 'applyfuns' is correct when asked to apply\n\
                  \'return :: a -> Maybe a\n\
                  \ return x = Just x'\n\
                  \and\n\
                  \'safeHead :: [a] -> Maybe a\n\
                  \ safeHead [] = Nothing\n\
                  \ safeHead (x:xs) = Just x'\n\
                  \onto an arbitrary non-Leaf tree of type 'Tree String [Int]'..."
  , successMsg  = "You got 5 marks because your 'applyfuns return safeHead\
                  \ (Fork l x r)' was correct."
  , failMsg     = "Your 'applyfuns return safeHead (Fork l x r)' was incorrect."
  , prop        = makeTernaryProp prop_applyfunsFork
  , condition   = Always
  }

--------------------------------------------------------------------------------
-- EXERCISE 2
--------------------------------------------------------------------------------

test_updateNodesEmpty = Test
  { mark        = 3
  , description = newSection ++ "Checking that 'updateNodes' works correctly on\n\
                  \'Empty :: BinTree Int'\nwith function '(+ 1) :: Int -> Int'\n\
                  \and arbitrary routes..."
  , successMsg  = "You got 3 marks because 'updateNodes' works correctly in \
                  \this case."
  , failMsg     = "Your function 'updateNodes' did not work correctly in \
                  \this case."
  , prop        = makeUnaryProp prop_updateNodesEmpty
  , condition   = Always
  }

test_updateNodesNode = Test
  { mark        = 3
  , description = "Checking that 'updateNodes' works correctly on the empty \
                  \route,\nnonempty trees of type 'BinTree String'\n\
                  \and function 'map toUpper :: String -> String'"
  , successMsg  = "You got 3 marks because 'updateNodes' works correctly in \
                  \this case."
  , failMsg     = "Your function 'updateNodes' did not work correctly in \
                  \this case."
  , prop        = makeTernaryProp prop_updateNodesNode
  , condition   = Always
  }

test_updateNodesFull = Test
  { mark        = 4
  , description = "Checking that 'updateNodes' works correctly on nonempty \
                  \routes\nwith trees of type 'BinTree String'\n\
                  \and function '(++) \"test\" :: String -> String'"
  , successMsg  = "You got 4 marks because 'updateNodes' works correctly in \
                  \this case."
  , failMsg     = "Your function 'updateNodes' did not work correctly in \
                  \this case."
  , prop        = makeBinaryProp prop_updateNodesFull
  , condition   = Always
  }

test_updateNodesPartial = Test
  { mark        = 2
  , description = "Checking that 'updateNodes' works correctly on nonempty \
                  \routes that do not exceed the tree\n\
                  \with trees of type 'BinTree String'\n\
                  \and function '(++) \"test\" :: String -> String'"
  , successMsg  = "You got 2 marks because 'updateNodes' works correctly in \
                  \this case."
  , failMsg     = "Your function 'updateNodes' did not work correctly in \
                  \this case."
  , prop        = makeBinaryProp prop_updateNodesPartial
  , condition   = IfFail test_updateNodesFull
  }

--------------------------------------------------------------------------------
-- EXERCISE 3
--------------------------------------------------------------------------------

evalAllMark :: Int
evalAllMark = 10

test_evalAll :: Test
test_evalAll = Test
  { mark        = evalAllMark
  , description = newSection ++ "Checking that your 'eval' function works correctly..."
  , successMsg  = "You got " ++ show evalAllMark ++ " marks because your 'eval'\
                  \ function worked correctly."
  , failMsg     = "Your 'eval' function did not work correctly."
  , prop        = makeUnaryPropWith prop_evalCorrect arbitrary shrink
  , condition   = Always
  }

evalDivFreeMark :: Int
evalDivFreeMark = 6

test_evalDivFree :: Test
test_evalDivFree = Test
  { mark        = evalDivFreeMark
  , description = "Checking that your 'eval' function works correctly on\
                  \ division-free calculator expressions..."
  , successMsg  = "You got " ++ show evalDivFreeMark ++ " marks because your\
                  \ 'eval' function worked correctly on division-free calculator\
                  \ expressions."
  , failMsg     = "Your 'eval' function did not work correctly on division-free\
                  \ calculator expressions."
  , prop        = makeUnaryPropWith prop_evalCorrect arbitraryDivFreeCalcExpr shrink
  , condition   = IfFail test_evalAll
  }

runAllMark :: Int
runAllMark = 10

test_runAll :: Test
test_runAll = Test
  { mark        = runAllMark
  , description = "Checking that your 'run' function works correctly (with initial store of 0)..."
  , successMsg = "You got " ++ show runAllMark ++ " marks because your 'run'\
                  \ function worked correctly."
  , failMsg     = "Your 'run' function did not work correctly."
  , prop        = makeUnaryPropWith prop_runCorrect arbitrary shrink
  , condition   = Always
  }

runDivFreeMark :: Int
runDivFreeMark = 6

test_runDivFree :: Test
test_runDivFree = Test
  { mark        = runDivFreeMark
  , description = "Checking that your 'run' function works correctly on\
                  \ division-free commands (with initial store of 0)..."
  , successMsg  = "You got " ++ show runDivFreeMark ++ " marks because your 'run'\
                  \ function worked correctly on division-free commands."
  , failMsg     = "Your 'run' function did not work correctly on division-free\
                  \ commands."
  , prop        = makeUnaryPropWith prop_runCorrect arbitraryDivFreeCalcCmd shrink
  , condition   = IfFail test_runAll
  }
