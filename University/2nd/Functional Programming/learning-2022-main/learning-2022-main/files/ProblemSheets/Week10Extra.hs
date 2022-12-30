module Week10Extra where

import System.Random
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Data.List

--
--  Additional Datatypes for practicing with Functor and Monad implementations
--

data BinT a =
  Leaf | Node (BinT a) a (BinT a)
  deriving Show

data RoseT a =
  Lf | Nd a [ RoseT a ]
  deriving Show

data ThreeT a =
  Lf3 | Nd3 a (RoseT (ThreeT a))
  deriving Show

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving Show

data Rose a = Br (a, [ Rose a ])

-- 
--  Cards and decks data-types and Show instances 
--

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK | RA
  deriving (Eq,Ord,Enum)
data Suit  = C | D | H | S
  deriving (Eq,Ord,Enum)
data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Eq)
type Deck = [Card]

standard52 :: Deck
standard52 = [Card {rank = r, suit = s} | r <- [R2 .. RA], s <- [C .. S]]

instance Show Rank where
  show R2 = "2"
  show R3 = "3"
  show R4 = "4"
  show R5 = "5"
  show R6 = "6"
  show R7 = "7"
  show R8 = "8"
  show R9 = "9"
  show R10 = "10"
  show RJ = "J"
  show RQ = "Q"
  show RK = "K"
  show RA = "A"

instance Show Suit where
  show C = "\9827"
  show D = "\9830"
  show H = "\9829"
  show S = "\9824"

red, black :: String -> String
red   s = "\x1b[31m" ++ s ++ "\x1b[0m"
black s = "\x1b[30m" ++ s ++ "\x1b[0m"

instance Show Card where
  show (Card { rank = r , suit = C }) = black (show r ++ show C)
  show (Card { rank = r , suit = D }) = red (show r ++ show D)
  show (Card { rank = r , suit = H }) = red (show r ++ show H)
  show (Card { rank = r , suit = S }) = black (show r ++ show S)

--
--  Picking Monad Definitions
-- 

class Monad m => PickingMonad m where
  pick :: Int -> Int -> m Int

instance PickingMonad Identity where
 pick lo hi = Identity lo

instance PickingMonad IO where
  pick lo hi | lo <= hi  = getStdRandom (randomR (lo, hi))
             | otherwise = error ("pick lo hi: lo = " ++ show lo ++ " is greater than hi = " ++ show hi)

instance PickingMonad [] where
  pick lo hi | lo <= hi  = [lo..hi]
             | otherwise = error ("pick lo hi: lo = " ++ show lo ++ " is greater than hi = " ++ show hi)

--
--  Probability Distribution Monad definition
-- 

newtype Dist a = Dist { dist :: [(a,Rational)] }  deriving (Show)

instance Monad Dist where
  return x = Dist [(x,1)]
  xm >>= f = Dist [(y,p*q) | (x,p) <- dist xm, (y,q) <- dist (f x)]

instance Functor Dist where
  fmap f xm = xm >>= return . f

instance Applicative Dist where
  pure = return
  xm <*> ym = xm >>= \x -> ym >>= return . x

instance PickingMonad Dist where
  pick lo hi | lo <= hi = Dist [(x,1 / fromIntegral (hi - lo + 1)) | x <- [lo..hi]]
             | otherwise = error ("pick lo hi: lo = " ++ show lo ++ " is greater than hi = " ++ show hi)


--
--  Short example: 'code' picks a random number i in 0-3 and returns hello[i]
--

code :: PickingMonad m => m Char
code = do
  i <- pick 0 3
  return ("hello" !! i)

--
--  Utility functions:
--    1. 'prob' computes the total probability of a value occurring in a given distribution
--    2. 'normalise' normalises a distribution by first computing the list of values 
--       in its support, and then returning the probabilities of all those values
--

prob :: Eq a => Dist a -> a -> Rational
prob xm x = sum [p | (y,p) <- dist xm, x == y]

normalise :: Eq a => Dist a -> Dist a
normalise xm = Dist [(x,prob xm x) | x <- support xm]
  where
    support :: Eq a => Dist a -> [a]
    support xm = nub [x | (x,p) <- dist xm, p > 0]  -- "nub" removes duplicates from a list

--
--  Binary trees, addresses and directions 
--

data Bin a = L a | B (Bin a) (Bin a)  deriving (Show,Eq)

type Direction = Either () ()
type Address   = [Direction]
