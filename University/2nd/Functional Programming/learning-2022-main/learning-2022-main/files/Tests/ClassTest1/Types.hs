{-# LANGUAGE Safe #-}

module Types where

import Data.Char

charLabel :: Char -> Int
charLabel char =  ord (toUpper char) - ord 'A'

key1 :: String
key1 = "LYKBDOCAWITNVRHJXPUMZSGEQF"

key2 :: String
key2 = "UDMZIQKLNJOSVETCYPBXAWRGHF"

plaintext1 :: String
plaintext1 = "The Quick Brown Fox Jumped Over The Lazy Dog"

plaintext2 :: String
plaintext2 = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

factors :: Int -> [Int]
factors n = [ k | k <- [1..n] , n `mod` k == 0 ]

isPrime :: Int -> Bool
isPrime n = factors n == [1, n]

data Direction = MoveLeft | MoveRight | MoveUp | MoveDown deriving (Eq, Show)

type Command = (Direction, Int)

exampleCommands :: [Command]
exampleCommands = [(MoveRight, 10), (MoveLeft, 5), (MoveUp, 20)]
