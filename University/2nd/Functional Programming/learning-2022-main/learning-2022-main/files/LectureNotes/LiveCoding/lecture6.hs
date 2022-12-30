--
--  lecture6.hs
--

import Data.List

-- f : a -> a
-- f^2 : a -> a
-- f^3 : a -> a
-- f^4 : a -> a
-- ...

--   f^0 = id  (\x .x)

zero :: (a -> a) -> (a -> a)
zero f x = x

one :: (a -> a) -> (a -> a)
one f x = f x

two :: (a -> a) -> (a -> a)
two f x = f (f x)

three :: (a -> a) -> (a -> a)
three f x = f (f (f x))

-- f^(n+1)(x) = f (f^n(x))
  
churchEncode :: Int -> ((a -> a) -> (a -> a))
churchEncode n | n == 0 = \f -> (\x -> x)
churchEncode n | n > 0 = \f -> \x -> f ((churchEncode (n-1) f) x)

type CE a = (a -> a) -> (a -> a)

-- f^(n+m)(x) = f^n(f^m(x))

add :: CE a -> CE a -> CE a
add m n = \f x -> (n f) ((m f) x)

-- f^(nm)(x) = (f^m(x))^n

mult :: CE a -> CE a -> CE a
mult m n = \f x -> (n (m f)) x


-- foldl, foldr

-- foldr :: (a -> b -> b) -> b -> [a] -> b

--- foldr f i [x1,x2,....,xn] = x1 `f` (x2 `f` ... (xn-1 `f` (xn `f` i)))
--- foldl f i [x1,x2,....,xn] = ((i `f` x1) `f` x2) .... `f` xn

-- foldr + 0 [1,2,3]    1:2:3:[]
-- 1 + (2 + (3 + 0))


-- assoc (a (*) b) (*) c = a (*) (b (*) c)
-- comm a (*) b = b (*) a 

-- +, *, and, or,
-- f \comp g 

filter' :: (a -> Bool) -> [a] -> [a]
filter' p l = 
  let op x r = if (p x) then x:r else r
      i = []

  in foldr op i l

  -- where -- op :: a -> [a] -> [a]
  --       op x r = if (p x) then x:r else r
        
  --       -- i :: [a]
  --       i = []

-- \x -> f x == f   "eta-expansion"

product :: Num a => [a] -> a
product = foldr (\ x y -> x + y) 1

-- Show how the list comprehension [(x,y) | x <- [1,2], y <- [3,4]]
-- with two generators can be re-expressed using two comprehensions
-- with single generators. Hint: nest one comprehension within the
-- other and make use of the library function concat :: [[a]] -> [a].

foo :: [(Int,Int)]
foo = [(x,y) | x <- [1,2], y <- [3,4]]

-- [[(1,3),(1,4)],[(2,3),(2,4)]]

foo' :: [(Int,Int)]
foo' = concat [ [ (x,y) | y <- [3,4] ] | x <- [1,2] ] 

-- data Maybe a = Nothing
--              | Just a 

-- find :: (a -> Bool) -> [a] -> Maybe a 


doLogin :: String -> String -> [(String,String)] -> IO ()
doLogin username password users =
  case find (\(un,_) -> username == un) users of
    Nothing -> putStrLn "User not found"
    Just (un,pw) -> if password == pw
                    then putStrLn "Login ok"
                    else putStrLn "Wrong password"


myUsers = [("Eric","Password"),("Martin","Haskell")]


-- 1 :: Int
-- (\x -> x) :: a -> a 
-- Lamdas are a way of writing elements of function types
