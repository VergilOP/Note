-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

import System.Random
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Data.List

import Week10Extra

--
--  Practicing with Type Constuctors, Functors and Monads
--

type F1 a = Maybe a
type F2 a = Either a String
type F3 a = [ a ]
type F4 a = BinT a
type F5 a = Int -> a
type F6 a = (a -> Int) -> Int
type F7 a = RoseT a
type F8 a = ThreeT a
type F9 a = Expr a

-- fmap :: (a -> b) -> f a -> f b

fmap1 :: (a -> b) -> F1 a -> F1 b
fmap1 f Nothing = Nothing
fmap1 f (Just x) = Just (f x)

fmap2 :: (a -> b) -> F2 a -> F2 b
fmap2 f (Left x) = Left (f x)
fmap2 f (Right s) = Right s 

fmap3 :: (a -> b) -> F3 a -> F3 b
fmap3 f [] = []
fmap3 f (x:xs) = (f x):(fmap3 f xs)

fmap4 :: (a -> b) -> F4 a -> F4 b
fmap4 f Leaf = Leaf
fmap4 f (Node l x r) = Node (fmap4 f l) (f x) (fmap4 f r)

fmap5 :: (a -> b) -> F5 a -> F5 b
fmap5 f r = \i -> f (r i)

fmap6 :: (a -> b) -> F6 a -> F6 b 
fmap6 f m kb = m (\a -> kb (f a))

fmap7 :: (a -> b) -> F7 a -> F7 b
fmap7 f Lf = Lf
fmap7 f (Nd x brs) = Nd (f x) (fmap3 (fmap7 f) brs)

fmap8 :: (a -> b) -> F8 a -> F8 b
fmap8 f Lf3 = Lf3
fmap8 f (Nd3 x brs) = Nd3 (f x) (fmap7 (fmap8 f) brs) 

fmap9 :: (a -> b) -> F9 a -> F9 b
fmap9 f (Var x) = Var (f x)
fmap9 f (Val i) = Val i
fmap9 f (Add l r) = Add (fmap9 f l) (fmap9 f r) 

-- pure :: a -> f a

pure1 :: a -> F1 a
pure1 x = Just x

pure2 :: a -> F2 a
pure2 x = Left x

pure3 :: a -> F3 a
pure3 x = [ x ] 

pure5 :: a -> F5 a
pure5 x = \i -> x 

pure6 :: a -> F6 a
pure6 x = \k -> k x 

pure9 :: a -> F9 a
pure9 x = Var x

-- (>>=) :: f a -> (a -> f b) -> f b

bind1 :: F1 a -> (a -> F1 b) -> F1 b
bind1 Nothing f = Nothing
bind1 (Just x) f = f x 

bind2 :: F2 a -> (a -> F2 b) -> F2 b
bind2 (Left x) f = f x
bind2 (Right s) f = Right s

bind3 :: F3 a -> (a -> F3 b) -> F3 b
bind3 [] f = []
bind3 (x:xs) f = f x ++ bind3 xs f

bind5 :: F5 a -> (a -> F5 b) -> F5 b
bind5 m f = \i -> f (m i) i

bind6 :: F6 a -> (a -> F6 b) -> F6 b
bind6 m f = \k -> m (\a -> f a k)

bind9 :: F9 a -> (a -> F9 b) -> F9 b
bind9 (Var x) f = f x
bind9 (Val i) f = Val i
bind9 (Add l r) f = Add (bind9 l f) (bind9 r f)

--
--  Using Monads
--

labelRoseS :: Rose a -> State Int (Rose (Int,a))
labelRoseS (Br (x,brs)) = do i <- get
                             modify ((+)1)
                             brs' <- sequence (map labelRoseS brs)
                             pure (Br ((i,x),brs'))

labelRose :: Rose a -> Rose (Int,a)
labelRose r = fst (runState (labelRoseS r) 0)


readAndRespond :: IO ()
readAndRespond = do putStrLn "Tell me something!"
                    s <- getLine
                    putStrLn $ "You said: " ++ s 

flipWords :: IO ()
flipWords = do putStrLn "Type some words (or press return to quit)."
               str <- getLine
               if (null str)
                 then return ()
                 else do putStrLn "Here are your words, backwards!"
                         putStrLn (unwords $ reverse $ words str)
                         flipWords

--
--  Using the Picking Monad
--

choose :: PickingMonad m => [a] -> m a
choose xs = do i <- pick 0 (length xs - 1)
               pure (xs !! i)

simulate :: Monad m => m Bool -> Integer -> m Integer
simulate m i | i <= 0    = pure 0
simulate m i | otherwise = do k <- simulate m (i-1)
                              b <- m
                              if b
                                then pure (k+1)
                                else pure k

cut :: PickingMonad m => [a] -> m ([a],[a])
cut cs = do i <- choose [0..length cs]
            pure (splitAt i cs)

shuffle :: PickingMonad m => ([a],[a]) -> m [a]
shuffle (xs,[])     = return xs
shuffle ([],ys)     = return ys
shuffle (x:xs,y:ys) = do
  let m = length (x:xs)
  let n = length (y:ys)
  k <- pick 1 (m + n)
  if k <= m
  then do
    zs <- shuffle (xs,y:ys)
    return (x:zs)
  else do
    zs <- shuffle (x:xs,ys)
    return (y:zs)

riffles :: PickingMonad m => ([a] -> m ([a],[a])) -> (([a],[a]) -> m [a]) -> Int -> [a] -> m [a]
riffles c s i d | i <= 0 = pure d
riffles c s i d | otherwise = do riffledDeck <- riffles c s (i-1) d
                                 cutDeck <- c riffledDeck
                                 s cutDeck

permute :: PickingMonad m => [a] -> m [a]
permute [] = pure []
permute (x:xs) = do perm <- permute xs
                    (l,r) <- cut perm
                    pure (l ++ [x] ++ r)

validAddresses :: Bin a -> [Address]
validAddresses (L _)   = [[]]
validAddresses (B l r) = [[]]
                      ++ [Left  ():ds | ds <- validAddresses l]
                      ++ [Right ():ds | ds <- validAddresses r]

genTree :: PickingMonad m => [a] -> m (Bin a)
genTree [] = error "Empty list"
genTree [x] = return (L x)
genTree (x:xs) = do
  t <- genTree xs
  ptr <- choose (validAddresses t)
  goLeft <- choose [True,False]
  return $ insertIntoTree x ptr goLeft t
    where
      insertIntoTree :: a -> Address -> Bool -> Bin a -> Bin a
      insertIntoTree y [] left t = if left then B (L y) t else B t (L y)
      insertIntoTree y (Left  ():ptr) left (B l r) = B (insertIntoTree y ptr left l) r
      insertIntoTree y (Right ():ptr) left (B l r) = B l (insertIntoTree y ptr left r)
      insertIntoTree y _              left (L _)   = error "invalid address"



