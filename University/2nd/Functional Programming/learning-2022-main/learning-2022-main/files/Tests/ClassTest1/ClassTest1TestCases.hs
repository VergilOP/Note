{-# LANGUAGE ParallelListComp #-}
module ClassTest1TestCases where

import MarkingCore

import TestCasesUtils
import qualified ClassTest1 as Student
import qualified ClassTest1Solutions as Solutions

import Test.QuickCheck

import Data.Int
import Data.List
import Control.DeepSeq

import Types


-------------------------------------------------------------------------------
-- Question 1: Parity
-------------------------------------------------------------------------------

arbitraryBit :: Gen Char
arbitraryBit = elements ['0','1']

arbitraryByte :: Gen String
arbitraryByte = vectorOf 8 arbitraryBit

arbitraryBitstringNotOfLengthDivisibleBy8 :: Gen String
arbitraryBitstringNotOfLengthDivisibleBy8 = do
 n <- elements $ [1..7] ++ [9..15] ++ [17..23]
 vectorOf n arbitraryBit

arbitraryEvenByte :: Gen String
arbitraryEvenByte =
 arbitraryByte `suchThat` (\bs -> even (length (filter (=='1') bs)))

arbitraryEvenBitstring :: Gen String
arbitraryEvenBitstring = do
 n <- elements [1..6]
 bs <- vectorOf n arbitraryEvenByte
 return (concat bs)

shrinkBitstring :: String -> [String]
shrinkBitstring [] = []
shrinkBitstring s = [take 8 s] ++ [drop 8 s] ++ [s' | s' <- shrink (drop 8 s)]

arbitraryOddByte :: Gen String
arbitraryOddByte =
 arbitraryByte `suchThat` (\bs -> odd (length (filter (=='1') bs)))

arbitraryOddBitstring :: Gen String
arbitraryOddBitstring = do
 n <- elements [1..6]
 bs <- vectorOf n arbitraryOddByte
 return (concat bs)


dropByteAt :: Int -> String -> String
dropByteAt i bs = let (xs, ys) = splitAt i bs in xs ++ drop 8 ys

shrinkByteString :: String -> [String]
shrinkByteString s
  | length s <= 8 = []
  | otherwise     = subterms ++ [s' | t <- subterms , s' <- shrinkByteString t]
                      where
                        n = length s `div` 8
                        subterms = [ dropByteAt (k * 8) s | k <- [0..pred n] ]

byteAt :: Int -> String -> String
byteAt i bs = let (_, ys) = splitAt i bs in take 8 ys

hasEvenNumOfOnes :: String -> Bool
hasEvenNumOfOnes s = even . length $ filter (== '1') s

shrinkByteStringEven :: String -> [String]
shrinkByteStringEven s
  | length s <= 8 = []
  | otherwise     =
      [ dropByteAt (k * 8) s | k <- [0..pred n],
                               hasEvenNumOfOnes (byteAt (k * 8) s) ]
        where
          n = length s `div` 8

-- Test that student solution returns True for the empty list.
prop_checkParity_empty :: Property
prop_checkParity_empty = Student.checkParity ([] :: [Char]) ~= True

-- Test that student solution returns True for input strings with length
-- divisible by 8.
prop_checkParity_length_positive :: String -> Property
prop_checkParity_length_positive s =
 (length s `mod` 8 == 0) ==> Student.checkParity s ~= True

-- Test that student solution returns False for input strings with length
-- not divisible by 8.
prop_checkParity_length_negative :: String -> Property
prop_checkParity_length_negative s =
 (length s `mod` 8 /= 0) ==> Student.checkParity s ~= False

-- Check that student solution returns True for input strings where every
-- byte has even parity.
prop_checkParity_even :: String -> Property
prop_checkParity_even s = Student.checkParity s ~= True

-- Check that student solution returns False for input strings where
-- at least one byte has odd parity.
prop_checkParity_odd :: String -> Property
prop_checkParity_odd s = Student.checkParity s ~= False

-------------------------------------------------------------------------------
-- Question 2: Substitution
-------------------------------------------------------------------------------

arbitraryKey :: Gen String
arbitraryKey = shuffle ['A'..'Z']

plaintextlength :: Int
plaintextlength = 150

lowercase :: String
lowercase = ['a'..'z']

uppercase :: String
uppercase = ['A'..'Z']

otherchars :: String
otherchars = [' ', '\'', ',', '!', '-', '?']

arbitraryUppercase :: Gen (String,String)
arbitraryUppercase = do
  pt <- vectorOf plaintextlength (elements uppercase)
  key <- arbitraryKey
  return (pt,key)

arbitraryLowercase :: Gen (String,String)
arbitraryLowercase = do
 pt <- vectorOf plaintextlength (elements lowercase)
 key <- arbitraryKey
 return (pt,key)

arbitraryLetters :: Gen (String,String)
arbitraryLetters = do
 pt <- vectorOf plaintextlength (elements (lowercase ++ uppercase))
 key <- arbitraryKey
 return (pt,key)

arbitraryPlaintext :: Gen (String,String)
arbitraryPlaintext = do
 pt <- vectorOf plaintextlength (elements (lowercase ++ uppercase ++ otherchars))
 key <- arbitraryKey
 return (pt,key)

splitIntoChunks30 :: String -> [String]
splitIntoChunks30 [] = []
splitIntoChunks30 s = [take 30 s] ++ splitIntoChunks30 (drop 30 s)

shrinkSubstitution :: (String , String) -> [(String,String)]
shrinkSubstitution (pt,key) = [(pt',key) | pt' <- splitIntoChunks30 pt]


-- Test that student solution correctly encrypts plaintext
-- which consists of only uppercase letters
prop_substitution_uppercase :: String -> String -> Property
prop_substitution_uppercase plaintext key =
 Student.substitution plaintext key ~= Solutions.substitution plaintext key

-- Test that student solution correctly encrypts plaintext
-- which consists of only lowercase letters
prop_substitution_lowercase :: String -> String -> Property
prop_substitution_lowercase plaintext key =
 Student.substitution plaintext key ~= Solutions.substitution plaintext key

-- Test that student solution correctly encrypts plaintext
-- which consists of uppercase and lowercase letters
prop_substitution_letters :: String -> String -> Property
prop_substitution_letters plaintext key =
 Student.substitution plaintext key ~= Solutions.substitution plaintext key

prop_substitution_all :: String -> String -> Property
prop_substitution_all plaintext key =
 Student.substitution plaintext key ~= Solutions.substitution plaintext key

-------------------------------------------------------------------------------
-- Question 3: Primes
-------------------------------------------------------------------------------

---------
-- Part a
---------

first100largestPrimes :: [Int]
first100largestPrimes = [3,5,7,7,11,13,13,17,19,19,23,23,23,29,31,31,31,37,37,41,43,43,47,47,47,53,53,53,59,61,61,61,67,67,71,73,73,73,79,79,83,83,83,89,89,89,89,97,97,101,103,103,107,109,109,113,113,113,113,113,113,113,127,127,131,131,131,137,139,139,139,139,139,149,151,151,151,157,157,157,163,163,167,167,167,173,173,173,179,181,181,181,181,181,191,193,193,197,199,199,199]

next200largestPrimes :: [Int]
next200largestPrimes = [199,199,199,199,199,199,211,211,211,211,211,211,223,223,227,229,229,233,233,233,239,241,241,241,241,241,251,251,251,257,257,257,263,263,263,269,271,271,271,277,277,281,283,283,283,283,283,293,293,293,293,293,293,293,307,307,311,313,313,317,317,317,317,317,317,317,331,331,331,337,337,337,337,337,347,349,349,353,353,353,359,359,359,359,367,367,367,373,373,373,379,379,383,383,383,389,389,389,389,397,397,401,401,401,401,409,409,409,409,409,419,421,421,421,421,421,431,433,433,433,439,439,443,443,443,449,449,449,449,457,457,461,463,463,467,467,467,467,467,467,479,479,479,479,487,487,491,491,491,491,499,499,503,503,503,509,509,509,509,509,509,521,523,523,523,523,523,523,523,523,523,541,541,541,547,547,547,547,547,557,557,557,563,563,563,569,571,571,571,577,577,577,577,577,587,587,587,593,593,593,599]

largestPrimes500to1000 :: [Int]
largestPrimes500to1000 = [997,997,997,997,997,1009,1009,1013,1013,1013,1019,1021,1021,1021,1021,1021,1031,1033,1033,1033,1039,1039,1039,1039,1039,1049,1051,1051,1051,1051,1051,1061,1063,1063,1063,1069,1069,1069,1069,1069,1069,1069,1069,1069,1087,1087,1091,1093,1093,1097,1097,1097,1103,1103,1103,1109,1109,1109,1109,1117,1117,1117,1123,1123,1123,1129,1129,1129,1129,1129,1129,1129,1129,1129,1129,1129,1151,1153,1153,1153,1153,1153,1163,1163,1163,1163,1171,1171,1171,1171,1171,1181,1181,1181,1187,1187,1187,1193,1193,1193,1193,1201,1201,1201,1201,1201,1201,1213,1213,1217,1217,1217,1223,1223,1223,1229,1231,1231,1231,1237,1237,1237,1237,1237,1237,1249,1249,1249,1249,1249,1259,1259,1259,1259,1259,1259,1259,1259,1259,1277,1279,1279,1283,1283,1283,1289,1291,1291,1291,1297,1297,1301,1303,1303,1307,1307,1307,1307,1307,1307,1319,1321,1321,1321,1327,1327,1327,1327,1327,1327,1327,1327,1327,1327,1327,1327,1327,1327,1327,1327,1327,1361,1361,1361,1367,1367,1367,1373,1373,1373,1373,1381,1381,1381,1381,1381,1381,1381,1381,1381,1399,1399,1399,1399,1399,1409,1409,1409,1409,1409,1409,1409,1423,1423,1427,1429,1429,1433,1433,1433,1439,1439,1439,1439,1447,1447,1451,1453,1453,1453,1459,1459,1459,1459,1459,1459,1471,1471,1471,1471,1471,1481,1483,1483,1487,1489,1489,1493,1493,1493,1499,1499,1499,1499,1499,1499,1511,1511,1511,1511,1511,1511,1523,1523,1523,1523,1531,1531,1531,1531,1531,1531,1543,1543,1543,1549,1549,1553,1553,1553,1559,1559,1559,1559,1567,1567,1571,1571,1571,1571,1579,1579,1583,1583,1583,1583,1583,1583,1583,1597,1597,1601,1601,1601,1607,1609,1609,1613,1613,1613,1619,1621,1621,1621,1627,1627,1627,1627,1627,1637,1637,1637,1637,1637,1637,1637,1637,1637,1637,1657,1657,1657,1663,1663,1667,1669,1669,1669,1669,1669,1669,1669,1669,1669,1669,1669,1669,1693,1693,1697,1699,1699,1699,1699,1699,1709,1709,1709,1709,1709,1709,1721,1723,1723,1723,1723,1723,1733,1733,1733,1733,1741,1741,1741,1747,1747,1747,1753,1753,1753,1759,1759,1759,1759,1759,1759,1759,1759,1759,1777,1777,1777,1783,1783,1787,1789,1789,1789,1789,1789,1789,1801,1801,1801,1801,1801,1811,1811,1811,1811,1811,1811,1823,1823,1823,1823,1831,1831,1831,1831,1831,1831,1831,1831,1847,1847,1847,1847,1847,1847,1847,1861,1861,1861,1867,1867,1871,1873,1873,1877,1879,1879,1879,1879,1879,1889,1889,1889,1889,1889,1889,1901,1901,1901,1907,1907,1907,1913,1913,1913,1913,1913,1913,1913,1913,1913,1931,1933,1933,1933,1933,1933,1933,1933,1933,1949,1951,1951,1951,1951,1951,1951,1951,1951,1951,1951,1951,1973,1973,1973,1979,1979,1979,1979,1987,1987,1987,1993,1993,1997,1999]

-- Check if student returns correct answer for input 2.
prop_largestPrimeBetween_2 :: Property
prop_largestPrimeBetween_2 = Student.largestPrimeBetween 2 ~= 3

prop_largestPrimeBetween_first100 :: Property
prop_largestPrimeBetween_first100 =
 [deepseq (Student.largestPrimeBetween n) (Student.largestPrimeBetween n) | n <- [2..102]] ~= first100largestPrimes

prop_largestPrimeBetween_next200 :: Property
prop_largestPrimeBetween_next200
 = [deepseq (Student.largestPrimeBetween n) (Student.largestPrimeBetween n) | n <- [100..300]] ~= next200largestPrimes

---------
-- Part b
---------

-- First 50 strong primes
first50strongPrimes = [11,17,29,37,41,59,67,71,79,97,101,107,127,137,149,163,179,191,197,223,227,239,251,269,277,281,307,311,331,347,367,379,397,419,431,439,457,461,479,487,499,521,541,557,569,587,599,613,617,631]

-- Check if student solution is correct on empty list
prop_strongPrimes_empty :: Property
prop_strongPrimes_empty = Student.strongPrimes 0 ~= []

-- Check if student solutions gives the first strong prime
prop_strongPrimes_correct_1 :: Property
prop_strongPrimes_correct_1 = Student.strongPrimes 1 ~= [11]

-- Check if student solutions gives the first fifty strong primes
prop_strongPrimes_correct_50 :: Property
prop_strongPrimes_correct_50 = p_permOf (Student.strongPrimes 50) first50strongPrimes

--------------------------------------------------------------------------------
-- Question 4: Directions
--------------------------------------------------------------------------------

instance Arbitrary Direction where

  arbitrary = elements [MoveLeft, MoveRight, MoveUp, MoveDown]

arbitraryCommand :: Gen Command
arbitraryCommand = do
  k <- chooseInt (0, 10)
  d <- arbitrary
  return (d, k)

arbitraryCommandList :: Gen [Command]
arbitraryCommandList = chooseInt (1, 25) >>= \n -> vectorOf n arbitraryCommand

arbitraryCoordinate :: Gen (Int, Int)
arbitraryCoordinate = do
  x <- chooseInt (-100, 100)
  y <- chooseInt (-100, 100)
  return (x, y)

arbitraryCommandListAndCoord :: Gen ([Command], (Int, Int))
arbitraryCommandListAndCoord = do
  cs    <- arbitraryCommandList
  coord <- arbitraryCoordinate
  return (cs, coord)

arbitraryLengthAndCoord :: Gen (Int, (Int, Int))
arbitraryLengthAndCoord = do
  k     <- chooseInt (0, 10)
  coord <- arbitraryCoordinate
  return (k, coord)

prop_executeCommandsCorrect :: [Command] -> (Int, Int) -> Property
prop_executeCommandsCorrect cs coord =
  Student.executeCommands cs coord ~= Solutions.executeCommands cs coord

prop_executeCommandsSingleDirCorrect :: Direction -> Int -> (Int, Int) -> Property
prop_executeCommandsSingleDirCorrect d n coord =
  counterexample s comp
    where
      s = "Your 'executeCommands' function did not work correctly on inputs "
          ++ " [(" ++ show d ++ ", " ++ show n ++ ")]" ++ " and " ++ show coord ++ ""
      comp = Student.executeCommands [(d, n)] coord ~= Solutions.executeCommands [(d, n)] coord

--------------------------------------------------------------------------------
-- Question 5: ATM
--------------------------------------------------------------------------------

arbitraryDenomination :: Gen Int
arbitraryDenomination =
  elements [10, 50, 100, 200, 500]

arbitraryDenominationList :: Gen [Int]
arbitraryDenominationList = do
  k  <- chooseInt (1, 6)
  ds <- vectorOf k arbitraryDenomination
  return $ sort (nub ds)

arbitraryDenominationListAndAmount :: Gen (Int, [Int])
arbitraryDenominationListAndAmount = do
  ds  <- arbitraryDenominationList
  k   <- chooseInt (1, 4)
  ns  <- vectorOf k (elements ds)
  return (maximum ds + sum ns, ds)

arbitraryDenominationListAndAmount' :: Gen (Int, [Int])
arbitraryDenominationListAndAmount' = do
  let ds = [10, 50, 100, 200, 600, 1200]
  d   <- elements ds
  let as = d : filter (> d) ds
  k   <- chooseInt (1, 5)
  return (k * sum as, as)

sumsUpTo :: [(Int, Int)] -> Int -> Bool
sumsUpTo xs a = sum [ k * n | (n, k) <- xs ] == a

showArithmeticExpr :: [(Int, Int)] -> String
showArithmeticExpr xs =
  concat $ intersperse " + " [ "(" ++ show k ++ "*" ++ show n ++ ")" | (n, k) <- xs ]

prop_atmChangeCorrect :: Int -> [Int] -> Property
prop_atmChangeCorrect n ds =
  if stu == sol then
    property True
  else
    counterexample s False
  where
    stu = deepseq (Student.atmChange n ds)   (Student.atmChange n ds)
    sol = deepseq (Solutions.atmChange n ds) (Solutions.atmChange n ds)
    s   = "For amount " ++ show n ++ " and list of denominations " ++ show ds ++
          ", your implementation of 'atmChange' returned " ++ show stu ++
          " but the correct answer is " ++ show sol ++ "."

prop_atmChangeSound :: Int -> [Int] -> Property
prop_atmChangeSound n ds =
  let
    result = deepseq (Student.atmChange n ds) (Student.atmChange n ds)
    s0     = "For amount " ++ show n ++ " and list of denominations " ++ show ds
  in
    -- The returned list might be correct but might be using denominations
    -- outside the given list.
    if not (all (\n -> n `elem` ds) (fst <$> result)) then
      let
        s = s0 ++ ", your 'atmChange' returned a list containing a denomination\
                  \ that is not in the given list of denominations."
      in
        counterexample s False
    -- The returned list might not be giving the correct sum.
    else if not (result `sumsUpTo` n) then
      let
        s = s0 ++ ", your 'atmChange' function returned " ++ show result
            ++ " but " ++ showArithmeticExpr result ++ " ≠ " ++ show n
      in counterexample s False
    -- If the previous two cases are not the case, the result should be correct.
    else
      property True
