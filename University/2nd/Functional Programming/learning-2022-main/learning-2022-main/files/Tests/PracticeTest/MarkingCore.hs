{-|
  This provides a framework for writing markin scripts.
  To write a marking script, you need to import this module in your file and
  call the function 'runMarking' in your main function on a list of 'Test's.
-}
module MarkingCore(
  Test(Test, mark, description, successMsg, failMsg, prop, condition)
, TestCondition(Always, IfFail, IfSuccess, Any, All)
, TestProperty
, makeNullaryProp
, makeUnaryProp
, makeBinaryProp
, makeTernaryProp
, makeUnaryPropWith
, makeBinaryPropWith
, makeTernaryPropWith
, makeQuarternaryPropWith
, makeNullaryProp'
, makeUnaryProp'
, makeBinaryProp'
, makeTernaryProp'
, makeUnaryPropWith'
, makeBinaryPropWith'
, makeTernaryPropWith'
, makeQuarternaryPropWith'
, runMarking
, newSection
) where

import Control.Monad
import System.Environment
import Test.QuickCheck
import Test.QuickCheck.Random

{-|
  Contains the data of a test:

    * the number of marks for this test,

    * a description that will be displayed before the test is run,

    * a success or fail message that will be displayed when the test passes or fails,

    * an object of type 'TestProperty' containing the logic of the test, and

    * an object of type 'TestCondition' specifying when the test should be run.
-}
data Test = Test { mark :: Int
                 , description :: String
                 , successMsg :: String
                 , failMsg :: String
                 , prop :: TestProperty
                 , condition :: TestCondition
                 }

{-|
  Specifies when a test should be run depending on the results of the tests that
  have been previously run. For instance, you can have a test A with condition
  @Any [IfFail B, IfSuccess C]@ for some tests B and C. Then test A will only be
  run if either test B has been run and failed or if test C has been run and passed.
-}
data TestCondition = Always
                   | IfFail Test
                   | IfSuccess Test
                   | Any [TestCondition]
                   | All [TestCondition]

{-|
  Evaluates a condition and returns a boolean. Besides the condition, this takes
  another two arguments: 'passed' and 'failed'. These are the tests that have
  already passed or failed respectively. See 'runAllTestsIter' for how this is used.
-}
evalCond :: TestCondition -> [Test] -> [Test] -> Bool
evalCond (Always) _ _ = True
evalCond (IfFail test) _ failed = test `elem` failed
evalCond (IfSuccess test) passed _ = test `elem` passed
evalCond (Any conds) passed failed = any (\cond -> evalCond cond passed failed) conds
evalCond (All conds) passed failed = all (\cond -> evalCond cond passed failed) conds

{-|
  A wrapper for QuickCheck test. To create objects of this type, you must use
  one of the functions provided, such as 'makeUnaryProp' or 'makeBinaryPropWith'.
-}
newtype TestProperty = TestProperty Property

instance Eq Test where
  t1 == t2 = description t1 == description t2

{-|
  The default timeout used for the tests. You shouldn't change this as 1 second
  works quite well for most tests. If you need a different timeout for some of
  your tests, use functions such as 'makeUnaryProp'' or 'makeBinaryPropWith''.
-}
defaultTime :: Int
defaultTime = 1000000

counterexampleMsg :: Int -> String
counterexampleMsg arity = case arity of
  0 -> ""
  1 -> "The test failed on input:"
  otherwise -> "The test failed on inputs:"

{-|
  Creates a nullary test property with a timeout of 1 second.
-}
makeNullaryProp
  :: Testable prop =>
     prop -> TestProperty
makeNullaryProp test = makeNullaryProp' test defaultTime

{-|
  Creates an unary test property. This does two things:

    * add a timeout of 1 second using 'within' in a clever way, and

    * add a simple counterexample messages using 'counterexample'.
-}
makeUnaryProp
  :: (Arbitrary a, Show a, Testable prop) =>
     (a -> prop) -> TestProperty
makeUnaryProp test = makeUnaryProp' test defaultTime

{-|
  Creates a binary test property. This does two things:

    * add a timeout of 1 second using 'within' in a clever way, and

    * add a simple counterexample messages using 'counterexample'.
-}
makeBinaryProp
  :: (Arbitrary a, Arbitrary b,
      Show a, Show b, Testable prop) =>
     (a -> b -> prop) -> TestProperty
makeBinaryProp test = makeBinaryProp' test defaultTime

{-|
  Creates a tertiary test property. This does two things:

    * add a timeout of 1 second using 'within' in a clever way, and

    * add a simple counterexample messages using 'counterexample'.
-}
makeTernaryProp
  :: (Arbitrary a, Arbitrary b, Arbitrary c,
      Show a, Show b, Show c, Testable prop) =>
     (a -> b -> c -> prop) -> TestProperty
makeTernaryProp test = makeTernaryProp' test defaultTime


{-|
  Creates a unary test property with a custom generator and shrink function.

  This is useful when the valid inputs form a small subset of the input type,
  e.g. you may want arbitrary permutations of @[1..n]@ instead of arbitrary lists.
  In these cases, discarding invalid inputs in the test itself would not work.
-}
makeUnaryPropWith
  :: (Show a, Testable prop) =>
     (a -> prop)
     -> Gen a -- generator
     -> (a -> [a]) -- shrink function
     -> TestProperty
makeUnaryPropWith test = makeUnaryPropWith' test defaultTime

{-|
  Creates a binary test property with a custom generator and shrink function.

  Note that this takes a single generator of type @Gen (a, b)@ instead of many
  generators, which is useful if you have many inputs that depend on each other.
  For example, suppose you have a test that takes a game board and a legal move
  for that board. Discarding the illegal moves using a precondtion will probably
  not work as that will exceed the discards limit, so you should instead write a
  generator that produces an arbitrary board *and* a legal move at the same time.
-}
makeBinaryPropWith
  :: (Show a, Show b, Testable prop) =>
     (a -> b -> prop)
     -> Gen (a, b) -- generator
     -> ((a, b) -> [(a, b)]) -- shrink function
     -> TestProperty
makeBinaryPropWith test = makeBinaryPropWith' test defaultTime

{-|
  Creates a tertiary test property with a custom generator and shrink function.

  Note that this takes a single generator of type @Gen (a, b, c)@ instead of many
  generators, which is useful if you have many inputs that depend on each other.
  For example, suppose you have a test that takes a game board and a legal move
  for that board. Discarding the illegal moves using a precondtion will probably
  not work as that will exceed the discards limit, so you should instead write a
  generator that produces an arbitrary board *and* a legal move at the same time.
-}
makeTernaryPropWith
  :: (Show a, Show b, Show c, Testable prop) =>
     (a -> b -> c -> prop)
     -> Gen (a, b, c) -- generator
     -> ((a, b, c) -> [(a, b, c)]) -- shrink function
     -> TestProperty
makeTernaryPropWith test = makeTernaryPropWith' test defaultTime

{-|
  Creates a quaternary test property with a custom generator and shrink function.
-}
makeQuarternaryPropWith
  :: (Show a, Show b, Show c, Show d, Testable prop) =>
     (a -> b -> c -> d -> prop)
     -> Gen (a, b, c, d) -- generator
     -> ((a, b, c, d) -> [(a, b, c, d)]) -- shrink function
     -> TestProperty
makeQuarternaryPropWith test = makeQuarternaryPropWith' test defaultTime


{-|
  Just like 'makeNullaryProp' but uses a custom timeout instead of the default one (1 second).
-}
makeNullaryProp'
  :: Testable prop =>
     prop -> Int -> TestProperty
makeNullaryProp' test time =
  TestProperty $ within time test

{-|
  Just like 'makeUnaryProp' but uses a custom timeout instead of the default one (1 second).
-}
makeUnaryProp'
  :: (Arbitrary a, Show a, Testable prop) =>
     (a -> prop) -> Int -> TestProperty
makeUnaryProp' test time =
  TestProperty $ counterexample (counterexampleMsg 1) $
    property (\x -> within time $ test x)

{-|
  Just like 'makeBinaryProp' but uses a custom timeout instead of the default one (1 second).
-}
makeBinaryProp'
  :: (Arbitrary a, Arbitrary b,
      Show a, Show b, Testable prop) =>
     (a -> b -> prop) -> Int -> TestProperty
makeBinaryProp' test time =
  TestProperty $ counterexample (counterexampleMsg 2) $
    property (\x y -> within time $ test x y)

{-|
  Just like 'makeTernaryProp' but uses a custom timeout instead of the default one (1 second).
-}
makeTernaryProp'
  :: (Arbitrary a, Arbitrary b, Arbitrary c,
      Show a, Show b, Show c, Testable prop) =>
     (a -> b -> c -> prop) -> Int -> TestProperty
makeTernaryProp' test time =
  TestProperty $ counterexample (counterexampleMsg 3) $
    property (\x y z -> within time $ test x y z)

{-|
  Just like 'makeUnaryPropWith' but uses a custom timeout instead of the default one (1 second).
-}
makeUnaryPropWith'
  :: (Show a, Testable prop) =>
     (a -> prop)
     -> Int -- timeout
     -> Gen a -- generator
     -> (a -> [a]) -- shrink function
     -> TestProperty
makeUnaryPropWith' test time gen shrink =
  TestProperty $ counterexample (counterexampleMsg 1) $
    forAllShrink gen shrink (\x -> within time $ test x)

{-|
  Just like 'makeBinaryPropWith' but uses a custom timeout instead of the default one (1 second).
-}
makeBinaryPropWith'
  :: (Show a, Show b, Testable prop) =>
     (a -> b -> prop)
     -> Int -- timeout
     -> Gen (a, b) -- generator
     -> ((a, b) -> [(a, b)]) -- shrink function
     -> TestProperty
makeBinaryPropWith' test time gen shrink =
  TestProperty $ counterexample (counterexampleMsg 2) $
    forAllShrink gen shrink (\(x, y) -> within time $ test x y)

{-|
  Just like 'makeTernaryPropWith' but uses a custom timeout instead of the default one (1 second).
-}
makeTernaryPropWith'
  :: (Show a, Show b, Show c, Testable prop) =>
     (a -> b -> c -> prop)
     -> Int -- timeout
     -> Gen (a, b, c) -- generator
     -> ((a, b, c) -> [(a, b, c)]) -- shrink function
     -> TestProperty
makeTernaryPropWith' test time gen shrink =
  TestProperty $ counterexample (counterexampleMsg 3) $
    forAllShrink gen shrink (\(x, y, z) -> within time $ test x y z)

{-|
  Just like 'makeQuaternaryPropWith' but uses a custom timeout instead of the default one (1 second).
-}
makeQuarternaryPropWith'
  :: (Show a, Show b, Show c, Show d, Testable prop) =>
     (a -> b -> c -> d -> prop)
     -> Int -- timeout
     -> Gen (a, b, c, d) -- generator
     -> ((a, b, c, d) -> [(a, b, c , d)]) -- shrink function
     -> TestProperty
makeQuarternaryPropWith' test time gen shrink =
  TestProperty $ counterexample (counterexampleMsg 4) $
    forAllShrink gen shrink (\(x, y, z, w) -> within time $ test x y z w)

{-|
  Runs the marking. It takes a list of tests to run and a boolean specifying
  whether this assignment is assessed (if true, the script will print the total
  number of marks at the end). You should run this in your 'main' function.
-}
runMarking :: [Test] -> Bool -> IO ()
runMarking tests assessed = do
  feedback <- getArgs >>= return . not . ("--marking" `elem`)
  marks <- runAllTests tests feedback
  when assessed $ putStrLn $ if feedback
    then newSection ++ newSection ++ newSection ++ (toMarks $ printMarks marks)
    else show marks

{-|
  Runs a list of tests and returns the total number of marks. This also
  takes a boolean specifying whether the script should print any feedback.
-}
runAllTests :: [Test] -> Bool -> IO Int
runAllTests tests feedback = runAllTestsIter 0 [] [] tests
  where
    runAllTestsIter :: Int -> [Test] -> [Test] -> [Test] -> IO Int
    runAllTestsIter marks successful failed [] = return marks
    runAllTestsIter marks successful failed (test:tests) = do
      let b = evalCond (condition test) successful failed
      result <- if b then runTest test feedback else return False
      if not b then
        runAllTestsIter marks successful failed tests
      else if result then
        runAllTestsIter (mark test + marks) (test:successful) failed tests
      else
        runAllTestsIter marks successful (test:failed) tests

{-|
  Runs a test in QuickCheck and returns whether that test passed. This also
  takes a boolean specifying whether the script should print any feedback.
-}
runTest :: Test -> Bool -> IO Bool
runTest test feedback = do
  when feedback $ putStrLn $ toBold $ description test
  let TestProperty x = prop test
  result <- isSuccess <$> quickCheckWithResult (makeQuickCheckArgs feedback) x
  when feedback $ putStrLn $ if result
    then toCorrect (successMsg test) ++ "\n"
    else toFail (failMsg test) ++ if null (failMsg test) then "" else "\n"
  return result

{-|
  The arguments we feed to QuickCheck when running the marking script. Note that
  we fix a seed to make sure the marking is deterministic, so do not change it!
-}
makeQuickCheckArgs :: Bool -> Args
makeQuickCheckArgs feedback = Args { replay = Just (mkQCGen 28, 0)
                                   , maxSuccess = 100
                                   , maxDiscardRatio = 10
                                   , maxSize = 30
                                   , chatty = feedback
                                   , maxShrinks = 30
                                   }

toMarks :: String -> String
toMarks s = "\x1b[1m\x1b[34m" ++ s ++ "\x1b[0m"

toBold :: String -> String
toBold s = "\x1b[1m" ++ s ++ "\x1b[0m"

toCorrect :: String -> String
toCorrect s = "\x1b[1m\x1b[32m" ++ s ++ " :)" ++ "\x1b[0m"

toFail :: String -> String
toFail s = "\x1b[1m\x1b[31m" ++ s ++ "\x1b[0m"

{-|
  Prints a section divider line. Use this before test descriptions.
-}
newSection :: String
newSection = take 80 (repeat '-') ++ "\n"

printMarks :: Int -> String
printMarks m = "You got " ++ show m ++ " out of " ++ show maxScore
                          ++ " i.e. " ++ show (m * 2) ++ "%. "
                          ++ line
  where
    maxScore = 50
    p = (fromIntegral m) / (fromIntegral maxScore)
    line :: String
    line | p < 0.4   = "Keep at it! ;)"
         | p < 0.5   = "You are on the right track now! :)"
         | p < 0.6   = "Good effort! :)"
         | p < 0.7   = "Job well done! :)"
         | p < 0.8   = "Really nice work! :)"
         | p < 0.9   = "Terrific! :D"
         | p < 1     = "Very impressive, nearly perfect! :D"
         | p == 1    = "Superb, absolutely perfect! :D"
         | otherwise = error "This shouldn't happen."
