-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
module ClassTest2 (dec2FactString , factString2Dec , treeByLevels, rotateLeft, rotateRight, leafIndices, treeToParens, parensToTree, phoneToString, stringToPhone, fingerTaps, facHelper, factorial', applyfuns, updateNodes, eval, run) where

import Types

import Control.Monad.Except
import Control.Monad.State

import Data.Char
import Data.List

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

-- Actually They are my Solution

-- Exercise 1

-- https://www.codewars.com/kata/54e320dcebe1e583250008fd/haskell

dec2FactString :: Integer -> String
dec2FactString n = go newlist n
    where newlist = reverse (takeWhile (<=n) (map factorial [0..]))
    
go :: [Integer] -> Integer -> String
go (x:[]) _ = "0"
go (x:xs) n = num : go xs (n `mod` x)
    where num = (['0'..'9'] ++ ['A'..'Z']) !! (fromIntegral (n `div` x))

factString2Dec :: String -> Integer
factString2Dec [] = 0
factString2Dec (x:xs)
    | len == 1 = 0
    | elem x ['0'..'9'] = fromIntegral (digitToInt x) * (factorial (len-1)) + factString2Dec xs
    | otherwise = fromIntegral num * (factorial (len-1)) + factString2Dec xs
    where len = fromIntegral(length (x:xs))
          (num, _) = head (filter (\(a,b)->b==x) (zip [10..35] ['A'..'Z']))

-- Exercise 2

-- https://www.codewars.com/kata/52bef5e3588c56132c0003bc

treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels = concat . go
  where
    go Nothing = [[]]
    go (Just (TreeNode l r v)) = [v] : zipappend (go l) (go r)
    zipappend []       yss      = yss
    zipappend xss      []       = xss
    zipappend (xs:xss) (ys:yss) = (xs ++ ys) : zipappend xss yss

-- Exercise 3

-- https://www.codewars.com/kata/5a6de0ec0136a1761d000093

rotateLeft,rotateRight :: Tree a -> Tree a

rotateLeft Empty = Empty
rotateLeft (Node l Empty x) = Node l Empty x
rotateLeft (Node l (Node rl rr rx) x) = Node (Node l rl x) rr rx

rotateRight Empty = Empty
rotateRight (Node Empty r x) = (Node Empty r x)
rotateRight (Node (Node ll lr lx) r x) = Node ll (Node lr r x) lx

-- Exercise 4

-- Answer from tutor
-- https://git.cs.bham.ac.uk/fp/learning-2022/-/blob/main/files/ProblemSheets/solutions/ProblemSheet-Week7-8-solutions.md

leafIndicesAcc :: Int -> BT a -> (BT (Int,Int) , Int)
leafIndicesAcc i Empty' = (Empty',i+1)
leafIndicesAcc i (Fork _ l r) =
  let (l',i') = leafIndicesAcc i l
      (r',i'') = leafIndicesAcc i' r
  in (Fork (i,i''-1) l' r' , i'')

leafIndices :: BT a -> BT (Int, Int)
leafIndices t = fst $ leafIndicesAcc 0 t

-- Exercise 5

-- Answer from online
-- https://www.codewars.com/kata/5fdb81b71e47c6000d26dc4b/haskell

treeToParens :: Tree' -> String
treeToParens Leaf = ""
treeToParens (l :*: r) = "(" ++ treeToParens l ++ ")" ++ treeToParens r

parensToTree :: String -> Tree'
parensToTree s = fst $ iter s where
  iter ('(':s) = let (tl,s') = iter s
                     (tr,s'') = iter s' in (tl :*: tr, s'')
  iter s = (Leaf, tail s)

-- Exercise 6

-- Answer from tutor
-- https://git.cs.bham.ac.uk/fp/learning-2022/-/blob/main/files/ProblemSheets/Homework7/Homework7Solutions.hs

{- Question 6a -}
phoneKeyboard :: Button -> [Char]
phoneKeyboard '1' = "1"
phoneKeyboard '2' = "ABC2"
phoneKeyboard '3' = "DEF3"
phoneKeyboard '4' = "GHI4"
phoneKeyboard '5' = "JKL5"
phoneKeyboard '6' = "MNO6"
phoneKeyboard '7' = "PQRS7"
phoneKeyboard '8' = "TUV8"
phoneKeyboard '9' = "WXYZ9"
phoneKeyboard '0' = " 0"
phoneKeyboard '#' = ".,"
phoneKeyboard  _  = undefined -- avoid warnings

phoneToString :: [(Button, Presses)] -> Text
phoneToString bps = phoneToStringHelper bps False

-- The second argument indicates whether we need to capitalize or not.
phoneToStringHelper :: [(Button, Presses)] -> Bool -> Text
phoneToStringHelper []               _ = []
phoneToStringHelper (('*', p) : bps) _ = phoneToStringHelper
                                          bps ((p `mod` 2) == 1)
phoneToStringHelper ((b  , p) : bps) u = if   u
                                         then c         : r
                                         else toLower c : r
  where
    cs = phoneKeyboard b
    n  = length cs
    c  = cs !! ((p - 1) `mod` n)
    r  = phoneToStringHelper bps False

{- Question 6b -}
stringToPhone :: Text -> [(Button, Presses)]
stringToPhone = concatMap charToPhone

charToPhone :: Char -> [(Button, Presses)]
charToPhone c = if   isUpper c
                then ('*',1) : [(b,p)]
                else           [(b,p)]
  where
    -- Do brute force search to avoid writing everything out by hand :)
    (b,p) = head [(b',p'+1) | b' <- '#' : ['0'..'9']    ,
                              let cs = phoneKeyboard b' ,
                              let n  = length cs - 1    ,
                              p' <- [0..n]              ,
                              toUpper c == cs !! p']

{- Question 6c -}
fingerTaps :: Text -> Presses
fingerTaps t = sum (map snd (stringToPhone t))

-- For Ex7-10 answer from tutor
-- https://git.cs.bham.ac.uk/fp/learning-2022/-/blob/main/files/ProblemSheets/Homework9/Homework9Solutions.hs
-- Exercise 7

facHelper :: Integer -> State Integer ()
facHelper 0 = pure ()
facHelper n = do facHelper (n-1)
                 modify ((*) n)

factorial' :: Integer -> Integer
factorial' n = snd (runState (facHelper n) 1)

-- Exercise 8

applyfuns :: (a -> c) -> (b -> d) -> Tree'' a b -> Tree'' c d
applyfuns _ g (Leaf' y)     = Leaf' (g y)
applyfuns f g (Fork' l x r) = Fork' (applyfuns f g l) (f x) (applyfuns f g r)

-- Exercise 9

updateNodes :: Route -> (a -> a) -> BinTree a -> BinTree a
updateNodes _            _ Empty''        = Empty''
updateNodes []           f (Node' l x r) = Node' l (f x) r
updateNodes (GoLeft :ds) f (Node' l x r) = Node' (updateNodes ds f l) (f x) r
updateNodes (GoRight:ds) f (Node' l x r) = Node' l (f x) (updateNodes ds f r)

-- Exercise 10

eval :: MonadError String m => CalcExpr -> m Int
eval (Val i) = pure i
eval (Add e f) = do ev <- eval e
                    fv <- eval f
                    return (ev + fv)
eval (Mult e f) = do ev <- eval e
                     fv <- eval f
                     return (ev * fv)
eval (Sub e f) = do ev <- eval e
                    fv <- eval f
                    return (ev - fv)
eval (Div e f) = do ev <- eval e
                    fv <- eval f
                    if (fv == 0)
                      then throwError "Divide by zero!"
                      else return (ev `div` fv)


run :: (MonadState Int m, MonadError String m) => CalcCmd -> m ()
run EnterC = pure ()
run (StoreC i cs) = do put i
                       run cs
run (AddC i cs) = do modify ((+) i)
                     run cs
run (MultC i cs) = do modify ((*) i)
                      run cs
run (SubC i cs) = do modify (\x -> x - i)
                     run cs
run (DivC i cs) | i == 0 = throwError "Divide by zero!"
                | otherwise = do modify (`div` i)
                                 run cs

