module ClassTest2TestCases where

import MarkingCore

import TestCasesUtils
import qualified ClassTest2 as Student
import qualified ClassTest2Solutions as Solutions

import Test.QuickCheck
import Control.DeepSeq
import Control.Monad.Writer

import Types

-------------------------------------------------------------------------------
-- Question 1: Tribonnaci
-------------------------------------------------------------------------------

arbitraryTribInt :: Gen Integer
arbitraryTribInt = chooseInteger (1,500)

trib750 :: Integer
trib750 = 1032087793261458151146144253319051102965775803285330460664754039961306892607834383198049583447207214324937718205487646152078664474941845692141827074614430709589728932871416797917511267155029606587673

trib1000 :: Integer
trib1000 = 1499952522327196729941271196334368245775697491582778125787566254148069690528296568742385996324542810615783529390195412125034236407070760756549390960727215226685972723347839892057807887049540341540394345570010550821354375819311674972209464069786275283520364029575324

prop_stateTrib_small :: Integer -> Property
prop_stateTrib_small n = Student.runStateTrib n ~= Solutions.runStateTrib n

prop_stateTrib_750 :: Property
prop_stateTrib_750 = s ~= trib750
 where
  s = deepseq (Student.runStateTrib 750) (Student.runStateTrib 750)

prop_stateTrib_1000 :: Property
prop_stateTrib_1000 = s ~= trib1000
 where
  s = deepseq (Student.runStateTrib 1000) (Student.runStateTrib 1000)

-------------------------------------------------------------------------------
-- Question 2: Writer/Leaves
-------------------------------------------------------------------------------

sizedArbitraryBin :: (Arbitrary a, Arbitrary b) => Int -> Gen (Bin a b)
sizedArbitraryBin 0 = Lf <$> arbitrary
sizedArbitraryBin n = frequency [ (4, genNode)
                                , (1, Lf <$> arbitrary) ]
  where
    genNode = do
      l <- sizedArbitraryBin (n `div` 2)
      r <- sizedArbitraryBin (n `div` 2)
      x <- arbitrary
      return $ Nd x l r

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bin a b) where
  arbitrary = sized sizedArbitraryBin

  shrink (Lf x)      = [Lf x]
  shrink (Nd x l r)  = [l, r] ++ [ Nd x' l' r' | (x', l', r') <- shrink (x, l, r) ]

instance (NFData a , NFData b) => NFData (Bin a b) where
  rnf (Lf x)     = rnf x
  rnf (Nd x l r) = rnf x `seq` rnf l `seq` rnf r

prop_writeLeavesFull :: Bin Bool Int -> Property
prop_writeLeavesFull t = stu ~= sol
  where
    stu = snd (runWriter (Student.writeLeaves   t))
    sol = snd (runWriter (Solutions.writeLeaves t))

simpleTree :: Bin Int Int
simpleTree = Nd 1 (Nd 1 (Lf 1) (Lf 1)) (Lf 1)

fromEither :: Either a a -> a
fromEither (Left  x) = x
fromEither (Right x) = x

fromEithers :: [Either a a] -> [a]
fromEithers = map fromEither

prop_writeLeavesSimple :: Property
prop_writeLeavesSimple = fromEithers stu ~= sol
  where
    stu = snd (runWriter (Student.writeLeaves simpleTree))
    sol = [1, 1, 1, 1, 1]

arbitraryLfForWriteLeaves :: Gen (Bin Int Bool)
arbitraryLfForWriteLeaves = do
  n <- arbitrary
  return $ Lf n

prop_writeLeavesLeaf :: Bin Int Bool -> Property
prop_writeLeavesLeaf t@(Lf n) = snd (runWriter (Student.writeLeaves t)) ~= [Left n]

-------------------------------------------------------------------------------
-- Question 3: Collapsing
-------------------------------------------------------------------------------

arbitraryLf :: Gen (Bin (Bin Int Bool) Bool)
arbitraryLf = do
  n <- arbitrary
  return $ Lf (Lf n)

arbitraryNdForCollapse :: Gen (Bin (Bin Bool Int) Int)
arbitraryNdForCollapse = do
  n <- arbitrary
  x <- arbitrary
  y <- arbitrary
  return $ Nd n (Lf (Lf x)) (Lf (Lf y))

prop_collapseFull :: Bin (Bin Int Bool) Bool -> Property
prop_collapseFull t = stu ~= sol
  where
    stu = Student.collapse   t
    sol = Solutions.collapse t

prop_collapseLf :: Bin (Bin Int Bool) Bool -> Property
prop_collapseLf t@(Lf (Lf n)) = stu ~= Lf n
  where
    stu = Student.collapse t

prop_collapseNd :: Bin (Bin Bool Int) Int -> Property
prop_collapseNd t@(Nd n (Lf (Lf x)) (Lf (Lf y))) = stu ~= Nd n (Lf x) (Lf y)
  where
    stu = Student.collapse t


-------------------------------------------------------------------------------
-- Question 4: Mapping with Addresses
-------------------------------------------------------------------------------

instance NFData Direction where
  rnf L = ()
  rnf R = ()

toReverseAddress :: Bin (a,Address) b -> Bin (a,Address) b
toReverseAddress (Lf (a,addr)) = (Lf (a,reverse addr))
toReverseAddress (Nd y l r) = Nd y (toReverseAddress l) (toReverseAddress r)

prop_mapLeavesWithAddressLeaf :: Property
prop_mapLeavesWithAddressLeaf =
 (Student.mapLeavesWithAddress (,) t) ~= Lf (5,[])
  where
   t :: Bin Int Bool
   t = Lf 5

prop_mapLeavesWithAddressCorrect :: Bin Int Bool -> Property
prop_mapLeavesWithAddressCorrect t = s ~= s'
  where
    s = Student.mapLeavesWithAddress (,) t
    s' = Solutions.mapLeavesWithAddress (,) t

prop_mapLeavesWithAddressCorrectReverse :: Bin Int Bool -> Property
prop_mapLeavesWithAddressCorrectReverse t = toReverseAddress s ~= s'
  where
    s = Student.mapLeavesWithAddress (,) t
    s' = Solutions.mapLeavesWithAddress (,) t



-------------------------------------------------------------------------------
-- Question 5: QuadTrees
-------------------------------------------------------------------------------

instance NFData QuadTree where
  rnf (P p) = rnf p
  rnf (N nw ne sw se) = rnf nw `seq`
                        rnf ne `seq`
                        rnf sw `seq`
                        rnf se

arbitraryPixel :: Gen Integer
arbitraryPixel = elements [0..255]

arbitraryImage :: Gen Image
arbitraryImage = do
  n  <- elements [2 ^ m | m <- [1..5]]
  vectorOf n (vectorOf n arbitraryPixel)

shrinkImage :: Image -> [Image]
shrinkImage [[p]] = []
shrinkImage [[p1,p2],[p3,p4]] = []
shrinkImage im = [c1,c2,c3,c4] ++ concat [[c1',c2',c3',c4'] | c1' <- shrinkImage c1
                                                            , c2' <- shrinkImage c2
                                                            , c3' <- shrinkImage c3
                                                            , c4' <- shrinkImage c4]
  where
    n = length (head im) `div` 2
    c1 = [take n ps | ps <- take n im]
    c2 = [drop n ps | ps <- take n im]
    c3 = [take n ps | ps <- drop n im]
    c4 = [drop n ps | ps <- drop n im]

arbitraryTree :: Int -> Gen QuadTree
arbitraryTree n | n > 0 = do nw <- arbitraryTree (n `div` 4)
                             ne <- arbitraryTree (n `div` 4)
                             sw <- arbitraryTree (n `div` 4)
                             se <- arbitraryTree (n `div` 4)
                             return (N nw ne sw se)
                | otherwise = do p <- arbitraryPixel
                                 return (P p)

shrinkQuadTree :: QuadTree -> [QuadTree]
shrinkQuadTree (P p) = []
shrinkQuadTree (N (P _) (P _) (P _) (P _)) = []
shrinkQuadTree (N nw ne sw se) = [nw,ne,sw,se] ++ concat [[nw',ne',sw',se'] | nw' <- shrinkQuadTree nw
                                                                           , ne' <- shrinkQuadTree ne
                                                                           , sw' <- shrinkQuadTree sw
                                                                           , se' <- shrinkQuadTree se]

height :: QuadTree -> [Int]
height (P p) = [0]
height (N nw ne sw se) = map (+1) (concat [height x | x <- [nw,sw,ne,se]])

heightOneBranch :: QuadTree -> Int
heightOneBranch (P p) = 0
heightOneBranch (N nw ne sw se) = 1 + heightOneBranch nw

arbitraryLargerQuadTree :: Gen QuadTree
arbitraryLargerQuadTree = suchThat (sized arbitraryTree) (\qt -> heightOneBranch qt > 0)

balanced :: QuadTree -> Bool
balanced qt = and $ map (uncurry (==))  $ (,) <$> hs <*> hs
  where
    hs = height qt

instance Arbitrary QuadTree where
  arbitrary = sized arbitraryTree
  shrink    = shrinkQuadTree

prop_image_all :: Image -> Property
prop_image_all im = (Student.toQuadTree im) ~= (Solutions.toQuadTree im)

prop_quadtree_all :: QuadTree -> Property
prop_quadtree_all qt = classify (balanced qt) "Balanced"
  ((Student.fromQuadTree qt) ~= (Solutions.fromQuadTree qt))

prop_quadtreeimage :: QuadTree -> Property
prop_quadtreeimage qt = (Student.toQuadTree $ Student.fromQuadTree qt) ~= qt

prop_imagequadtree :: Image -> Property
prop_imagequadtree im = (Student.fromQuadTree $ Student.toQuadTree im) ~= im
