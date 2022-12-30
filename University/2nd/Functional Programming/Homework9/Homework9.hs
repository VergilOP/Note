-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE FlexibleContexts #-}

module Homework9 (applyfuns, updateNodes, eval, run) where

import Types

import Control.Monad.Except
import Control.Monad.State

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

-- Question 0

facHelper :: Integer -> State Integer ()
facHelper n | n == 1 = pure ()
            | otherwise = do
                            modify (\x->x*n)
                            facHelper (n-1)

factorial :: Integer -> Integer
factorial n = snd (runState (facHelper n) 1)

-- Question 1
applyfuns :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d
applyfuns f1 f2 (Leaf x) = Leaf (f2 x)
applyfuns f1 f2 (Fork l x r) = Fork (applyfuns f1 f2 l) (f1 x) (applyfuns f1 f2 r)

-- Question 2
updateNodes :: Route -> (a -> a) -> BinTree a -> BinTree a
updateNodes xs f Empty = Empty
updateNodes [] f (Node l n r) = Node l (f n) r
updateNodes (x:xs) f (Node l n r)
        | x == GoLeft = Node (updateNodes xs f l) (f n) r
        | otherwise = Node l (f n) (updateNodes xs f r)

-- Question 3
eval :: MonadError String m => CalcExpr -> m Int
eval (Val x) = pure x
eval (Div l (Val 0)) = throwError "Divide by zero!"
eval (Add l r) = do 
                    a <- eval l
                    b <- eval r
                    pure (a+b)
eval (Mult l r) = do 
                    a <- eval l
                    b <- eval r
                    pure (a*b)
eval (Div l r) = do 
                    a <- eval l
                    b <- eval r
                    pure (a `div` b)
eval (Sub l r) = do 
                    a <- eval l
                    b <- eval r
                    pure (a-b)
            

run :: (MonadState Int m, MonadError String m) => CalcCmd -> m ()
run EnterC = pure ()
run (DivC 0 y) = throwError "Divide by zero!"
run (StoreC x y) = do put x
                      run y
run (AddC x y)   = do modify (+x)
                      run y
run (MultC x y)   = do modify (*x)
                       run y
run (DivC x y)   = do modify (`div` x)
                      run y
run (SubC x y)   = do modify (\z->z-x)
                      run y
