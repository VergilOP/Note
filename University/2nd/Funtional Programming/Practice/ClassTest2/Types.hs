-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
---------------------------------------------------------------------------------
-------------------------- DO **NOT** MODIFY THIS FILE --------------------------
---------------------------------------------------------------------------------


module Types where

import Control.Monad.Except
import Control.Monad.State

-- Exercise 1

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

-- Exercise 2

data TreeNode a = TreeNode {
  left  :: Maybe (TreeNode a),
  right :: Maybe (TreeNode a),
  value :: a
  } deriving Show

-- Exercise 3

data Tree a = Empty
            | Node { left', right' :: Tree a , value' :: a }
            deriving (Show,Eq,Foldable)

-- Exercise 4

data BT a = Empty' | Fork a (BT a) (BT a)

-- Exercise 5

data Tree' = Leaf | Tree' :*: Tree' deriving (Eq, Show)

-- Exercise 6

-- Valid buttons are ['0'..'9']++['*','#']
type Button = Char

-- Valid presses are [1..]
type Presses = Int

-- Valid text consists of
-- ['A'..'Z']++['a'...'z']++['0'..'9']++['.',',',' ']
type Text = String

-- Exercise 8

data Tree'' a b = Leaf' b | Fork' (Tree'' a b) a (Tree'' a b)
  deriving (Eq, Show)

str2int :: String -> Int
str2int xs = length xs

int2bool :: Int -> Bool
int2bool n = n /= 0

-- Exercise 9

data BinTree a = Empty'' | Node' (BinTree a) a (BinTree a)
  deriving (Eq, Show)

data Direction = GoLeft | GoRight
  deriving (Eq, Show, Bounded, Enum)

type Route = [Direction]

-- Exercise 10

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
