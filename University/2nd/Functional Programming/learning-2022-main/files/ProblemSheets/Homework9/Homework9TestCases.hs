{-# LANGUAGE StandaloneDeriving #-}

module Homework9TestCases where

import Types
import TestCasesUtils
import qualified Homework9 as Student
import qualified Homework9Solutions as Solutions

import Data.Char
import Test.QuickCheck
import Control.Monad.Except
import Control.Monad.State
import Control.DeepSeq

--------------------------------------------------------------------------------
-- EXERCISE 0
--------------------------------------------------------------------------------

prop_fac_1 :: Property
prop_fac_1 = Student.factorial 1 ~= 1

prop_fac_25 :: Property
prop_fac_25 = Student.factorial 25 ~= 15511210043330985984000000

prop_fac_50 :: Property
prop_fac_50 = Student.factorial 50 ~= 30414093201713378043612608166064768844377641568960512000000000000

--------------------------------------------------------------------------------
-- EXERCISE 1
--------------------------------------------------------------------------------

instance (NFData a , NFData b) => NFData (Tree a b) where
  rnf (Leaf x)     = rnf x
  rnf (Fork l x r) = rnf x `seq` rnf l `seq` rnf r

sizedArbitraryTree :: (Arbitrary a , Arbitrary b) => Int -> Gen (Tree a b)
sizedArbitraryTree 0 = Leaf <$> arbitrary
sizedArbitraryTree n = frequency [(4, genFork),
                                  (1, Leaf <$> arbitrary)]
  where
    genFork = do
      l <- sizedArbitraryTree (n `div` 2)
      r <- sizedArbitraryTree (n `div` 2)
      x <- arbitrary
      return (Fork l x r)

instance (Arbitrary a , Arbitrary b) => Arbitrary (Tree a b) where
  arbitrary = sized sizedArbitraryTree
  shrink (Leaf x) = [Leaf x]
  shrink (Fork l x r) =
    -- try subterms
    [l , r] ++
    -- recursively shrink all subterms
    [Fork l' x' r' | (l' , x' , r') <- shrink (l, x, r)]
    -- NB: The order matters: keep it as above.

prop_applyfunsLeaf :: [Int] -> Property
prop_applyfunsLeaf xs = Student.applyfuns (length :: [Int] -> Int) sum (Leaf xs)
                     ~= Leaf (sum xs)

toMaybe :: a -> Maybe a
toMaybe = return

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

prop_applyfunsFork :: Tree String [Int] -> String -> Tree String [Int] -> Property
prop_applyfunsFork l x r = Student.applyfuns toMaybe safeHead (Fork l x r)
                        ~= Fork (Student.applyfuns toMaybe safeHead l)
                                (toMaybe x)
                                (Student.applyfuns toMaybe safeHead r)

--------------------------------------------------------------------------------
-- EXERCISE 2
--------------------------------------------------------------------------------

prop_updateNodesEmpty :: Route -> Property
prop_updateNodesEmpty r = Student.updateNodes r (+ 1) (Empty :: BinTree Int)
                          ~= Empty

prop_updateNodesNode :: String -> BinTree String -> BinTree String -> Property
prop_updateNodesNode s l r = Student.updateNodes [] (map toUpper) (Node l s r)
                             ~= Node l (map toUpper s) r

prop_updateNodesFull :: Route -> BinTree String -> Property
prop_updateNodesFull r t =
  not (null r) ==>
    Student.updateNodes r ((++) "test") t
    ~= Solutions.updateNodes r ((++) "test") t

routeExceedsTree :: Route -> BinTree a -> Bool
routeExceedsTree []           _            = False
routeExceedsTree (_:_)        Empty        = True
routeExceedsTree (GoLeft :ds) (Node l _ _) = routeExceedsTree ds l
routeExceedsTree (GoRight:ds) (Node _ _ r) = routeExceedsTree ds r

prop_updateNodesPartial :: Route -> BinTree String -> Property
prop_updateNodesPartial r t =
  not (null r) && not (routeExceedsTree r t) ==>
       Student.updateNodes r ((++) "test") t
    ~= Solutions.updateNodes r ((++) "test") t

sizedArbitraryBinTree :: Arbitrary a => Int -> Gen (BinTree a)
sizedArbitraryBinTree 0 = return Empty
sizedArbitraryBinTree n = frequency [(4, genNode),
                                     (1, return Empty)]
  where
    genNode = do
      l <- sizedArbitraryBinTree (n `div` 2)
      r <- sizedArbitraryBinTree (n `div` 2)
      x <- arbitrary
      return (Node l x r)

instance Arbitrary a => Arbitrary (BinTree a) where
  arbitrary = sized sizedArbitraryBinTree
  shrink Empty = []
  shrink (Node l x r) =
    -- try a simpler constructor
    [Empty] ++
    -- try subterms
    [l , r] ++
    -- recursively shrink all subterms
    [Node l' x' r' | (l' , x' , r') <- shrink (l, x, r)]
    -- NB: The order matters: keep it as above.
    -- NB: Do *not* use
    -- [Branch x' l' r' | x' <- shrink x, l' <- shrink l, r' <- shrink r]
    -- as explained here:
    -- https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck-Arbitrary.html#g:1

instance NFData Direction where
  rnf _ = ()

instance Arbitrary Direction where
  arbitrary = oneof [return GoLeft , return GoRight]
  shrink = const []

instance NFData a => NFData (BinTree a) where
  rnf Empty        = ()
  rnf (Node l x r) = rnf l `seq` rnf x `seq` rnf r

--------------------------------------------------------------------------------
-- EXERCISE 3
--------------------------------------------------------------------------------

arbitrarySmallInt :: Gen Int
arbitrarySmallInt = chooseInt (0, 2000)

arbitraryVal :: Gen CalcExpr
arbitraryVal = Val <$> arbitrarySmallInt

data CalcExprOperator = AddOp | MultOp | DivOp | SubOp deriving (Eq, Show)

data CalcCmdOperator = ExprOp CalcExprOperator | StoreOp deriving (Eq, Show)

deriving instance Show CalcExpr
deriving instance Show CalcCmd

arbitraryOperator :: Gen CalcExprOperator
arbitraryOperator = elements [AddOp, MultOp, DivOp, SubOp]

makeOperationalExpression :: CalcExprOperator -> CalcExpr -> CalcExpr -> CalcExpr
makeOperationalExpression op e1 e2 = case op of
                                       AddOp  -> Add  e1 e2
                                       MultOp -> Mult e1 e2
                                       DivOp  -> Div  e1 e2
                                       SubOp  -> Sub  e1 e2

makeOperationalCommand :: CalcCmdOperator -> Int -> CalcCmd -> CalcCmd
makeOperationalCommand op n c = case op of
                                  ExprOp AddOp  -> AddC   n c
                                  ExprOp MultOp -> MultC  n c
                                  ExprOp DivOp  -> DivC   n c
                                  ExprOp SubOp  -> SubC   n c
                                  StoreOp       -> StoreC n c

sizedArbitraryCalcExpr :: [CalcExprOperator] -> Int -> Gen CalcExpr
sizedArbitraryCalcExpr ops 0 = arbitraryVal
sizedArbitraryCalcExpr ops n = frequency [(4, genNode), (1, arbitraryVal)]
  where
    genNode = do
      l  <- sizedArbitraryCalcExpr ops (n `div` 2)
      r  <- sizedArbitraryCalcExpr ops (n `div` 2)
      op <- elements ops
      return $ makeOperationalExpression op l r

instance NFData CalcExpr where
  rnf (Val n)      = rnf n
  rnf (Add e1 e2)  = rnf e1 `seq` rnf e2
  rnf (Mult e1 e2) = rnf e1 `seq` rnf e2
  rnf (Div e1 e2)  = rnf e1 `seq` rnf e2
  rnf (Sub e1 e2)  = rnf e1 `seq` rnf e2

instance Arbitrary CalcExpr where
  arbitrary = sized (sizedArbitraryCalcExpr [AddOp, MultOp, DivOp, SubOp])

  shrink (Val  n)     = [Val n]
  shrink (Add  e1 e2) = e1 : e2 : [Add  e1' e2' | (e1', e2') <- shrink (e1, e2)]
  shrink (Mult e1 e2) = e1 : e2 : [Mult e1' e2' | (e1', e2') <- shrink (e1, e2)]
  shrink (Div  e1 e2) = e1 : e2 : [Div  e1' e2' | (e1', e2') <- shrink (e1, e2)]
  shrink (Sub  e1 e2) = e1 : e2 : [Sub  e1' e2' | (e1', e2') <- shrink (e1, e2)]

arbitraryDivFreeCalcExpr :: Gen CalcExpr
arbitraryDivFreeCalcExpr = sized (sizedArbitraryCalcExpr [AddOp, MultOp, SubOp])

prop_evalCorrect :: CalcExpr -> Property
prop_evalCorrect e =
  case (stu, sol) of
    (Right e1, Right e2) -> stu ~= sol
    (Left   _, Left   _) -> property True
  where
    sol = deepseq (runExcept (Solutions.eval e)) (runExcept (Solutions.eval e))
    stu = deepseq (runExcept (Student.eval   e)) (runExcept (Student.eval   e))

instance NFData CalcCmd where
  rnf EnterC       = ()
  rnf (StoreC n c) = rnf n `seq` rnf c
  rnf (AddC   n c) = rnf n `seq` rnf c
  rnf (MultC  n c) = rnf n `seq` rnf c
  rnf (DivC   n c) = rnf n `seq` rnf c
  rnf (SubC   n c) = rnf n `seq` rnf c

sizedArbitraryCalcCmd :: [CalcCmdOperator] -> Int -> Gen CalcCmd
sizedArbitraryCalcCmd ops 0 = return EnterC
sizedArbitraryCalcCmd ops n = do
  k  <- arbitrarySmallInt
  op <- elements ops
  c  <- sizedArbitraryCalcCmd ops (n - 1)
  return $ makeOperationalCommand op k c

instance Arbitrary CalcCmd where
  arbitrary =
    sized
      (sizedArbitraryCalcCmd
         [ExprOp AddOp, ExprOp MultOp, ExprOp DivOp, ExprOp SubOp, StoreOp])

  shrink EnterC       = []
  shrink (AddC  n c)  = c : [AddC    n' c' | (n', c') <- shrink (n, c) ]
  shrink (MultC n c)  = c : [MultC   n' c' | (n', c') <- shrink (n, c) ]
  shrink (DivC  n c)  = c : [DivC    n' c' | (n', c') <- shrink (n, c) ]
  shrink (SubC  n c)  = c : [SubC    n' c' | (n', c') <- shrink (n, c) ]
  shrink (StoreC n c) = c : [StoreC  n' c' | (n', c') <- shrink (n, c) ]

prop_runCorrect :: CalcCmd -> Property
prop_runCorrect c =
  case (stu, sol) of
    (Right (_, m), Right (_, n)) -> m ~= n
    (Left _, Left _)             -> property True
  where
    sol = deepseq (runStateT ((Solutions.run c) :: CS ()) 0) (runStateT ((Solutions.run c) :: CS ()) 0)
    stu = deepseq (runStateT ((Student.run   c) :: CS ()) 0) (runStateT ((Student.run   c) :: CS ()) 0)

arbitraryDivFreeCalcCmd :: Gen CalcCmd
arbitraryDivFreeCalcCmd =
  sized
    (sizedArbitraryCalcCmd
       [ExprOp AddOp, ExprOp MultOp, ExprOp SubOp, StoreOp])
