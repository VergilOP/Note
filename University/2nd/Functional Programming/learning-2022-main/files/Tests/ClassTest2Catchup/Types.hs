{-# LANGUAGE Safe #-}

module Types where

-- Question 1

f :: Integer -> Integer
f 0 = 0
f 1 = 1
f 2 = 1
f n | n > 2 = 7 * f (n-3) + 3 * f (n-2) 

-- Question 2

data T a = Leaf 
         | Fork a (T a) (T a) (T a)
         deriving (Eq, Show)

t0 = Fork 4 Leaf (Fork 7 Leaf Leaf Leaf) (Fork 10 (Fork 3 Leaf Leaf Leaf) Leaf Leaf)
t1 = Fork 12 (Fork 5 Leaf Leaf (Fork 6 Leaf Leaf Leaf)) Leaf (Fork 2 Leaf (Fork 3 Leaf Leaf Leaf) Leaf)

-- Question 3

data Direction = L | C | R deriving (Eq, Show)
type Address = [Direction]

-- Question 4

magicSquare1 :: [[Int]]
magicSquare1 = [[8,1,6],[3,5,7],[4,9,2]]

magicSquare2 :: [[Int]]
magicSquare2 = [[1,2,3],[4,5,6],[7,8,9]]

-- Question 5

data Json = JNull
          | JStr String
          | JNum Float
          | JBool Bool
          | JArr [Json]
          | JObj [(String, Json)]
          deriving (Eq, Show)


json = JObj [ ("firstName" , JStr "John")
            , ("lastName" , JStr "Smith")
            , ("spouse" , JNull)
            , ("address" , JObj [ ("street" , JStr "21 2nd St.")
                                , ("city" , JStr "Denver")
                                , ("apartment no.", JNull)
                                ])
            ]

