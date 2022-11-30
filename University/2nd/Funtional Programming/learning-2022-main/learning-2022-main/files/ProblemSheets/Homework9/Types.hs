-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

---------------------------------------------------------------------------------
-------------------------- DO **NOT** MODIFY THIS FILE --------------------------
---------------------------------------------------------------------------------

module Types where

import Control.Monad.Except
import Control.Monad.State

-- Question 1
data Tree a b = Leaf b | Fork (Tree a b) a (Tree a b)
  deriving (Eq, Show)

str2int :: String -> Int
str2int xs = length xs

int2bool :: Int -> Bool
int2bool n = n /= 0

-- Question 2
data BinTree a = Empty | Node (BinTree a) a (BinTree a)
  deriving (Eq, Show)

data Direction = GoLeft | GoRight
  deriving (Eq, Show, Bounded, Enum)

type Route = [Direction]

-- Question 3
data CalcExpr =
    Val Int
  | Add CalcExpr CalcExpr
  | Mult CalcExpr CalcExpr
  | Div CalcExpr CalcExpr
  | Sub CalcExpr CalcExpr

type CM a = Either String a 

expr1 = Mult (Add (Val 4) (Val 7)) (Div (Val 10) (Val 2))
expr2 = Sub (Val 10) (Div (Val 14) (Val 0))



data CalcCmd = EnterC
             | StoreC Int CalcCmd
             | AddC Int CalcCmd
             | MultC Int CalcCmd
             | DivC Int CalcCmd
             | SubC Int CalcCmd

type CS a = StateT Int (Either String) a

cmd1 = StoreC 7 (AddC 14 (DivC 3 EnterC))
cmd2 = StoreC 10 (MultC 2 (DivC 0 EnterC))

