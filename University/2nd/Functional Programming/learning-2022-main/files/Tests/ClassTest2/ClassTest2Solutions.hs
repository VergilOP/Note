-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module ClassTest2Solutions (stateTrib, runStateTrib, writeLeaves, collapse, mapLeavesWithAddress, toQuadTree, fromQuadTree) where

import Data.List
import Data.Char

import Control.Monad.State
import Control.Monad.Writer

import Types

-- Question 1

{-
The solution to this question is analagous to a function in the Lecture Notes\
\ which computes Fibonnaci numbers efficiently using the state monad.

(https://git.cs.bham.ac.uk/fp/learning-2022/-/blob/main/files/LectureNotes/Sections/monads.md)

The state is initialised with (1,0,0), and the function runStateTrib returns\
\ the first element of the triple. 

The transition function computes the next Tribonacci number by adding together\
the previous three Tribonnaci numbers, and also shifts the two previous numbers\
\ to the other two variables, forgetting the Tribonnaci number at (n-3) which\
\ is no longer needed.

-}

stateTrib :: Integer -> State (Integer,Integer,Integer) ()
stateTrib 1 = return ()
stateTrib n = do
              modify (\(x,y,z) -> (x+y+z,x,y))
              stateTrib (n-1)

runStateTrib :: Integer -> Integer
runStateTrib n = let ((),(a,b,c)) = (runState (stateTrib n) (1,0,0)) in a

-- Question 2

{-

To log the values in a binary tree, we user the function 'tell' implemented by\
\ the writer monad. If we are at a leaf, we use Left because we have an element\
\ of type a, and at a node we use Right because we have an element of type b.

In the node case, we first recurse down the left branch of the tree, then\
\ perform the log, and finally recure down the right branch. This gives us the\
\ required in-order traversal. 

-}

writeLeaves :: Bin a b -> Writer [Either a b] ()
writeLeaves (Lf a) = tell [Left a]
writeLeaves (Nd b l r) = do writeLeaves l
                            tell [Right b]
                            writeLeaves r
                            pure ()

-- Question 3

{-

The collapse functions involves binary trees whose leaves contain binary trees.\
\ We want to promote the binary trees from the leaves to being subtrees. This\
\ is done by pattern matching. When we have a leaf, we return the tree\
\ contained in the leaf, and for nodes we just recursively apply the function\
\ to the left and right branches.

-}

collapse :: Bin (Bin a b) b -> Bin a b
collapse (Lf t) = t
collapse (Nd b l r) = Nd b (collapse l) (collapse r)

-- Question 4

{-

Two solutions are given for this question. 

The first solution uses a helper function which uses an acculator Address to 
\ store the current address. Whenever we traverse down the left or right\
\ branch, we simply append 'L' or 'R' respectively to the accumulator value.\
\ Finally, at the leaves we can apply the function f with the stored Address.

The second solution instead modifies the _function_ we will be applying. \The
modified function 'g' and 'h' are called depending on which direction we\
\ traverse, and 'L', 'R' are appended to the address respectively. 

-}

mapLeavesWithAddress :: (a -> Address -> c) -> Bin a b -> Bin c b
mapLeavesWithAddress = go []
  where go :: Address -> (a -> Address -> c) -> Bin a b -> Bin c b
        go addr f (Lf x)  = Lf (f x addr)
        go addr f (Nd y l r) = Nd y (go (addr ++ [L]) f l) (go (addr ++ [R]) f r)

mapLeavesWithAddress' :: (a -> Address -> c) -> Bin a b -> Bin c b
mapLeavesWithAddress' f (Lf a)       = Lf (f a [])
mapLeavesWithAddress' f (Nd b t1 t2) 
 = Nd b (mapLeavesWithAddress' g t1) (mapLeavesWithAddress' h t2)
  where
   g = \a addr -> f a (L:addr)
   h = \a addr -> f a (R:addr)

-- Question 5

{-

The final question asks us write two mutually inverse functions which map\
\ between images and quadtrees. To map to a quadtree, we first find the height\
\ of the image, and divide by 2. We use this to be able to find each quadrant\
\ of the tree, and recursively call 'toQuadTree' on each quadrant, respecting\
\ the convention stated in the question. A single pixel is stored as a leaf\
\ containing the pixel.

To map a quadtree to an image, it is more subtle. Again, the base case is easy\
\ because a leaf is just a single pixel image. In the inductive case, we\
\ retrieve our 4 quadrants by recursively calling 'fromQuadTree' on each\
\ branch. We consider the north and south of the image. To obtain the north\
\ and south from the respective east and west quadrants, we use zipWith (++).\
\ To combine the north and south, we simply concatenate the lists.

-}

toQuadTree :: Image -> QuadTree
toQuadTree [[p]] = P p
toQuadTree im    = N c1 c2 c3 c4
  where
    n  = length im `div` 2
    c1 = toQuadTree [take n ps | ps <- take n im]
    c2 = toQuadTree [drop n ps | ps <- take n im]
    c3 = toQuadTree [take n ps | ps <- drop n im]
    c4 = toQuadTree [drop n ps | ps <- drop n im]

fromQuadTree :: QuadTree -> Image
fromQuadTree (P p) = [[p]]
fromQuadTree (N nw ne sw se) = n ++ s
  where
    nw' = fromQuadTree nw
    ne' = fromQuadTree ne
    sw' = fromQuadTree sw
    se' = fromQuadTree se
    n   = zipWith (++) nw' ne'
    s   = zipWith (++) sw' se'