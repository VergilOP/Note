module ClassTest2Marking where

import MarkingCore
import ClassTest2TestCases
import qualified ClassTest2 as Student

import Test.QuickCheck
import Control.Monad

import Types

main :: IO ()
main = runMarking tests True
  where
    tests = [ test_runStateTrib_small
            , test_runStateTrib_750
            , test_runStateTrib_1000
            , test_writeLeavesFull
            , test_writeLeavesLf
            , test_writeLeavesSimple
            , test_collapseFull
            , test_collapseLf
            , test_collapseNd
            , test_mapLeavesWithAddressLeaf
            , test_mapLeavesWithAddressCorrect
            , test_mapLeavesWithAddressCorrectReverse
            , test_image_all
            , test_quadtree_all
            , test_imagequadtree
            , test_quadtreeimage
            ]

-------------------------------------------------------------------------------
-- Tests: Question 1: Tribonnaci
-------------------------------------------------------------------------------

test_runStateTrib_small = Test
  { mark        = 5
  , description = newSection
                  ++
                  "Checking that 'runStateTrib n' works correctly for n <= 500..."
  , successMsg  = "You got 5 marks because your implementation of 'stateTrib'\
                  \ let us correctly compute Tribonnaci numbers up to and\
                  \ including 500."
  , failMsg     = "Your 'stateTrib' function did not work correctly."
  , prop        = makeUnaryPropWith prop_stateTrib_small arbitraryTribInt (const [])
  , condition   = Always
  }

test_runStateTrib_750 = Test
  { mark        = 2
  , description = "Checking that 'runStateTrib 750' gives the 750th Tribonnaci\
                  \ number in under 1 second..."
  , successMsg  = "You got 2 marks because your 'stateTrib' implementation\
                   \ allowed us to compute the 750th Tribonnaci number in under\
                   \ one second."
  , failMsg     = "Your 'runStateTrib' implementation did not allow us to\
                   \ compute the 750th Tribonnaci number in under one second.\
                   \ There are two possible reasons for failure: it either took\
                   \ too long to run 'runStateTrib 750' or it gave the wrong\
                   \ number despite running fast enough."
  , prop        = makeNullaryProp prop_stateTrib_750
  , condition   = Always
  }

test_runStateTrib_1000 = Test
  { mark        = 3
  , description = "Checking that 'runStateTrib 1000' gives the 1000th Tribonnaci\
                  \ number in under 1 second..."
  , successMsg  = "You got 3 marks because your 'stateTrib' implementation\
                   \ allowed us to compute the 1000th Tribonnaci number in under\
                   \ one second."
  , failMsg     = "Your 'runStateTrib' implementation did not allow us to\
                   \ compute the 1000th Tribonnaci number in under one second.\
                   \ There are two possible reasons for failure: it either took too\
                   \ long to run 'runStateTrib 1000', or it gave\
                   \ the wrong number despite running fast enough."
  , prop        = makeNullaryProp prop_stateTrib_1000
  , condition   = Always
  }

-------------------------------------------------------------------------------
-- Tests: Question 2: Writer/Leaves
-------------------------------------------------------------------------------

test_writeLeavesFull :: Test
test_writeLeavesFull = Test
  { mark        = 10
  , description = newSection ++
                  "Checking that your 'writeLeaves' function works\
                  \ correctly on arbitrary inputs..."
  , successMsg  = "You got 10 marks because your 'writeLeaves' function worked\
                  \ correctly on arbitrary inputs."
  , failMsg     = "Your 'writeLeaves' function did not work correctly (on\
                  \ arbitrary inputs)."
  , prop        = makeUnaryProp prop_writeLeavesFull
  , condition   = Always
  }

test_writeLeavesLf :: Test
test_writeLeavesLf = Test
  { mark        = 3
  , description = "Checking that your 'writeLeaves' function works\
                  \ correctly on leaves..."
  , successMsg  = "You got 3 marks because your 'writeLeaves' function worked\
                  \ correctly on leaves."
  , failMsg     = "Your 'writeLeaves' function did not work correctly on leaves."
  , prop        = makeUnaryPropWith prop_writeLeavesLeaf arbitraryLfForWriteLeaves (const [])
  , condition   = IfFail test_writeLeavesFull
  }

test_writeLeavesSimple :: Test
test_writeLeavesSimple = Test
  { mark        = 4
  , description = "Checking that your 'writeLeaves' function gives a permutation\
                  \ of the correct list of logs when it is run on the following\
                  \ simple tree (of type 'Bin Int Int'):\n\n"
                  ++ "    'Nd 1 (Nd 1 (Lf 1) (Lf 1)) (Lf 1)'...\n"
  , successMsg  = "You got 4 marks because your 'writeLeaves' function worked\
                  \ correctly on the aforementioned simple tree."
  , failMsg     = "Your 'writeLeaves' function did not work correctly on the\
                  \ aforementioned simple tree."
  , prop        = makeNullaryProp prop_writeLeavesSimple
  , condition   = IfFail test_writeLeavesFull
  }

-------------------------------------------------------------------------------
-- Tests: Question 3: Collapsing
-------------------------------------------------------------------------------

test_collapseFull :: Test
test_collapseFull = Test
  { mark        = 10
  , description = newSection
                  ++ "Checking that your 'collapse' function works correctly on\
                  \ arbitrary inputs..."
  , successMsg  = "You got 10 marks because your 'collapse' function worked\
                  \ correctly on arbitrary inputs."
  , failMsg     = "Your 'collapse' function did not work correctly (on arbitrary\
                  \ inputs)."
  , prop        = makeUnaryProp prop_collapseFull
  , condition   = Always
  }

test_collapseLf :: Test
test_collapseLf = Test
  { mark        = 2
  , description = "Checking that your 'collapse' function works correctly on\
                  \ leaves (containing leaves)..."
  , successMsg  = "You got 2 marks because your 'collapse' function worked\
                  \ correctly on leaves (containing leaves)."
  , failMsg     = "Your 'collapse' function did not work correctly on leaves\
                  \ (containing leaves)."
  , prop        = makeUnaryPropWith prop_collapseLf arbitraryLf shrink
  , condition   = IfFail test_collapseFull
  }

test_collapseNd :: Test
test_collapseNd = Test
  { mark        = 3
  , description = "Checking that your 'collapse' function works correctly on\
                  \ nodes (containing leaves)..."
  , successMsg  = "You got 3 marks because your 'collapse' function worked\
                  \ correctly on nodes (containing leaves)."
  , failMsg     = "Your 'collapse' function did not work correctly on nodes\
                  \ (containing leaves)."
  , prop        = makeUnaryPropWith prop_collapseNd arbitraryNdForCollapse shrink
  , condition   = IfFail test_collapseFull
  }

-------------------------------------------------------------------------------
-- Tests: Question 4: Mapping with Addresses
-------------------------------------------------------------------------------

test_mapLeavesWithAddressLeaf = Test
  { mark        = 2
  , description = newSection ++
                  "Checking that your 'mapLeavesWithAddress' function works\
                  \ correctly on leaves..."
  , successMsg  = "You got 2 marks because your 'mapLeavesWithAddress' function\
                  \ worked correctly on leaves."
  , failMsg     = "Your 'mapLeavesWithAddress' function did not work correctly\
                  \ on leaves."
  , prop        = makeNullaryProp prop_mapLeavesWithAddressLeaf
  , condition   = Always
  }

test_mapLeavesWithAddressCorrect = Test
  { mark        = 8
  , description = "Checking that your 'mapLeavesWithAddress' function works\
                  \ correctly on arbitrary inputs..."
  , successMsg  = "You got 8 marks because your 'mapLeavesWithAddress' function\
                  \ worked correctly on arbitrary inputs."
  , failMsg     = "Your 'mapLeavesWithAddress' function did not work correctly\
                  \ on arbitrary inputs. Notice, however, that there is another\
                  \ test case that gives you the same number of marks if this\
                  \ problem occurred due to address reversal."
  , prop        = makeUnaryProp prop_mapLeavesWithAddressCorrect
  , condition   = Always
  }

test_mapLeavesWithAddressCorrectReverse = Test
  { mark        = 8
  , description = "Checking that your mapLeavesWithAddress' function works\
                   \ correctly using the reversed address convention..."
  , successMsg  = "You got 8 marks because your 'mapLeavesWithAddress' function\
                  \ worked correctly with the reversed address convention."
  , failMsg     = "Your function 'mapLeavesWithAddress' did not work\
                  \ correctly with the reverse address convention."
  , prop        = makeUnaryProp prop_mapLeavesWithAddressCorrectReverse
  , condition   = IfFail test_mapLeavesWithAddressCorrect
  }

-------------------------------------------------------------------------------
-- Tests: Question 5: QuadTrees
-------------------------------------------------------------------------------

test_image_all = Test
  { mark        = 3
  , description = newSection ++
                  "Checking that your 'toQuadTree' function maps images to\
                  \ quadtrees correctly..."
  , successMsg  = "You got 3 marks because your 'toQuadTree' function worked\
                  \ correctly on arbitrary inputs (correctly formatted)."
  , failMsg     = "Your 'toQuadTree' function did not work correctly on the\
                  \ aforementioned test cases."
  , prop        = makeUnaryPropWith prop_image_all arbitraryImage shrinkImage
  , condition   = Always
  }

test_quadtree_all = Test
  { mark        = 3
  , description = "Checking that your 'fromQuadTree' function maps quadtrees\
                  \ to images correctly..."
  , successMsg  = "You got 3 marks because your 'fromQuadTree' function worked\
                  \ correctly on arbitrary quadtrees (with the right shape)."
  , failMsg     = "Your 'fromQuadTree' function did not work correctly on the\
                  \ aforementioned test cases."
  , prop        = makeUnaryPropWith prop_quadtree_all arbitraryLargerQuadTree shrinkQuadTree
  , condition   = Always
  }

test_imagequadtree = Test
  { mark        = 2
  , description = "Checking that we get the same image after converting it to\
                  \ a quadtree (using your 'toQuadTree') and then back to an\
                  \ image (using your 'fromQuadTree') i.e. your 'fromQuadTree' is\
                  \ a left inverse of your 'toQuadTree'..."
  , successMsg  = "You got 2 marks because your 'fromQuadTree' function is a\
                  \ left inverse of your 'toQuadTree'."
  , failMsg     = "Your 'fromQuadTree' function is not a left inverse of your\
                  \ 'toQuadTree' function."
  , prop        = makeUnaryPropWith prop_imagequadtree arbitraryImage shrinkImage
  , condition   = Always
  }

test_quadtreeimage = Test
  { mark        = 2
  , description = "Checking that we get the same quadtree after converting it to\
                  \ an image (using your 'fromQuadTree') and then back to a\
                  \ quadtree (using your 'toQuadTree') i.e. your 'fromQuadTree' is\
                  \ the right inverse of your 'toQuadTree'..."
  , successMsg  = "You got 2 marks because your 'fromQuadTree' function is a\
                  \ right inverse of your 'toQuadTree'."
  , failMsg     = "Your 'fromQuadTree' is not a right inverse of your\
                  \ 'toQuadTree' function."
  , prop        = makeUnaryPropWith prop_quadtreeimage arbitraryLargerQuadTree shrinkQuadTree
  , condition   = Always
  }
