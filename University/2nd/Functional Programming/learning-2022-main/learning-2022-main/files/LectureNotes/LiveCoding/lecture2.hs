--
--  Functions
--

-- Attendance: 95 92 13 77

-- 1. Composition

exList :: [Int]
exList = [2,6,7,4,2,5]

removeLast :: [a] -> [a]
removeLast l = reverse (tail (reverse l))

removeLast' :: [a] -> [a]
removeLast' l = reverse $ tail $ reverse l

-- filter :: (Int -> Bool) -> [Int] -> [Int]

keepOdds :: [Int] -> [Int]
keepOdds l = reverse $ filter odd l

-- 2. If/Then/Else Gaurds

isBigString :: String -> String
isBigString s = if (length s > 4) then "Yes, it is" else "Nope."
  
isBigString' :: String -> String
isBigString' s | length s > 14 = "Yes, it is"
isBigString' s | length s < 3 = "Nope"
isBigString' s | otherwise = "Kinda"

isBigString'' :: String -> String
isBigString'' s =
  if (length s > 14) then reverse s else
    if (length s < 3) then s else
      "Kinda"

-- 3. Pattern Matching

not :: Bool -> Bool
not True = False
not False = True

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

assocAndFlip :: ((a,b),c) -> (a,c,b)
assocAndFlip ((x,y),z) = (x,z,y)

listLength :: [a] -> Int
listLength [] = 0
listLength (_:xs) = 1 + listLength xs

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

-- 4. Case Expressions

isEmpty' :: [a] -> Bool
isEmpty' l = case l of
               [] -> True
               _ -> False


and'' :: Bool -> Bool -> Bool
and'' x y = case x of
              True -> case y of
                        True -> True
                        False -> False
              False -> case y of
                         True -> False
                         False -> False
evenEls :: [a] -> [a]
evenEls [] = []
evenEls (_:[]) = []
evenEls (x:y:xs) = y:(evenEls xs)

-- [1,2,3,4]
-- x = 1
-- y = 2
-- xs = [3,4]

oddEls :: [a] -> [a]
oddEls [] = []
oddEls (x:[]) = x:[]
oddEls (x:y:xs) = x:(oddEls xs)

-- 5. Lambda expressions

double :: Int -> Int
double x = x + x

-- double' :: (Int -> Int)
-- double' = (\x -> x + x)

-- doubleAll :: [Int] -> [String]
-- doubleAll l =
--   map ((\x -> if (x > 3)
--          then 49
--          else 57) :: Int -> Int) l 

  -- where myFun :: Int -> String
  --       myFun x | x > 3 = "big number!"
  --       muFun x | otherwise = "lame"

-- 6. Operators and Sections


(+++) :: [Int] -> [Int] -> [Int]
(+++) x y = x ++ y ++ [1,2,3]

append :: [Int] -> [Int] -> [Int]
append x y = x +++ y 


multiplesOfFour :: [Int] -> [Int]
multiplesOfFour l = filter (\i -> i `mod` 4 == 0) l 

addSeven :: [Int] -> [Int]
addSeven l = map (7 +) l 

--
--  Type classes
--

class MyEq a where
  isEqual :: a -> a -> Bool
  isNotEqual :: a -> a -> Bool



instance MyEq Bool where
  isEqual True True = True
  isEqual False False = True
  isEqual _ _ = False
  
  isNotEqual True False = True
  isNotEqual False True = True
  isNotEqual _ _ = False
    


  
-- f, g :: (Int -> Int)
-- f == g

-- 1. Monoids
-- 2. Serializable
-- 3. Sized

