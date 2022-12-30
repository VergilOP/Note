{-# LANGUAGE Safe #-}

module Types where

import Data.Char

-- Question 1

trib :: Int -> Int
trib 1 = 1
trib 2 = 1
trib 3 = 2
trib n | n > 3 = trib (n-3) + trib (n-2) + trib (n-1)

-- Question 2

data Bin a b = Lf a
             | Nd b (Bin a b) (Bin a b)
             deriving (Eq, Show)

tr1 = Nd 'a' (Lf 4) (Nd 'b' (Lf 7) (Lf 2))
tr2 = Nd 3 (Nd 5 (Nd 7 (Lf 'a') (Lf 'b')) (Nd 8 (Lf 'c') (Lf 'd'))) (Nd 4 (Lf 'e') (Lf 'f'))

-- Question 3

tr3 :: Bin (Bin Int Char) Char
tr3 = Nd 'a' (Lf (Nd 'b' (Lf 2) (Lf 7))) (Lf (Nd 'c' (Lf 2) (Lf 7)))

-- Question 4

data Direction = L | R deriving (Eq, Show)
type Address = [Direction]

-- Question 5

type Pixel = Integer

type Image = [[Pixel]]

image1 = [[1,2],[3,4]]

image2 = [ [1,  2,  3,  4]
         , [5,  6,  7,  8]
         , [9,  10, 11, 12]
         , [13, 14, 15, 16] ]

data QuadTree = P Pixel
              | N QuadTree QuadTree QuadTree QuadTree
              deriving (Eq, Show)
