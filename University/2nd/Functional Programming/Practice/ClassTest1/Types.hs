{-# LANGUAGE Safe #-}

module Types where

import Data.Char

{- Question 1 -}

data Membership = Open | Senior deriving (Eq, Show)
