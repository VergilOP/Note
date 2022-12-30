-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE FlexibleContexts #-}

module Homework9Solutions (factorial, applyfuns, updateNodes, eval, run) where

import Types

import Control.Monad.Except
import Control.Monad.State

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

-- Question 0
facHelper :: Integer -> State Integer ()
facHelper 0 = pure ()
facHelper n = do facHelper (n-1)
                 modify ((*) n)

factorial :: Integer -> Integer
factorial n = snd (runState (facHelper n) 1)

-- Question 1
applyfuns :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d
applyfuns _ g (Leaf y)     = Leaf (g y)
applyfuns f g (Fork l x r) = Fork (applyfuns f g l) (f x) (applyfuns f g r)

-- Question 2
updateNodes :: Route -> (a -> a) -> BinTree a -> BinTree a
updateNodes _            _ Empty        = Empty
updateNodes []           f (Node l x r) = Node l (f x) r
updateNodes (GoLeft :ds) f (Node l x r) = Node (updateNodes ds f l) (f x) r
updateNodes (GoRight:ds) f (Node l x r) = Node l (f x) (updateNodes ds f r)

-- Question 3
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
