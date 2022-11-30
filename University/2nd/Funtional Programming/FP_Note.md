- [Note of FP](#note-of-fp)
  - [Week 1](#week-1)
    - [Types in Programming, types in Haskell](#types-in-programming-types-in-haskell)
      - [Why are types useful?](#why-are-types-useful)
      - [Type inference before evaluation](#type-inference-before-evaluation)
      - [Tour of Haskell types](#tour-of-haskell-types)
      - [A useful `ghci` command: `:info`](#a-useful-ghci-command-info)
    - [Polymorphism](#polymorphism)
  - [Week 2](#week-2)
    - [Type classes and instances](#type-classes-and-instances)
      - [The type class `Eq`](#the-type-class-eq)
    - [Functions in Haskell](#functions-in-haskell)
      - [Composing functions](#composing-functions)
      - [Conditionals](#conditionals)
      - [Guarded equations](#guarded-equations)
      - [Pattern matching](#pattern-matching)
      - [Lambda expressions](#lambda-expressions)
      - [Operators and sections](#operators-and-sections)
    - [More on type classes and instances](#more-on-type-classes-and-instances)
      - [The type `Ordering` and the typeclass `Ord`](#the-type-ordering-and-the-typeclass-ord)
    - [Type clases in more detail](#type-clases-in-more-detail)
      - [Motivation](#motivation)
  - [Week 3](#week-3)
    - [List Comprehensions](#list-comprehensions)
      - [Basic Concepts](#basic-concepts)
      - [Dependent Generators](#dependent-generators)
      - [Guards](#guards)
      - [The Zip Function](#the-zip-function)
      - [String Comprehensions](#string-comprehensions)
      - [Extended Programming Example - The Caesar Cipher](#extended-programming-example---the-caesar-cipher)
      - [Frequency Tables](#frequency-tables)
      - [Cracking the Cipher(硬破解,没有给出关键数)](#cracking-the-cipher硬破解没有给出关键数)
    - [Recursive Functions](#recursive-functions)
      - [Basic Concepts](#basic-concepts-1)
      - [Multiple Recursion](#multiple-recursion)
      - [Mutual Recursion](#mutual-recursion)
      - [Advice on Recursion](#advice-on-recursion)
    - [Higher-order Functions](#higher-order-functions)
      - [Basic Concepts](#basic-concepts-2)
      - [Processing Lists](#processing-lists)
      - [The Foldr Function](#the-foldr-function)
      - [The Foldl Function](#the-foldl-function)
      - [The Composition Operator](#the-composition-operator)
  - [Week 4](#week-4)
    - [User defined data types - part 1](#user-defined-data-types---part-1)
      - [User defined data types](#user-defined-data-types)
      - [Some important type constructors](#some-important-type-constructors)
      - [Lists revisited](#lists-revisited)
      - [Binary tree](#binary-tree)
    - [Lazy natural numbers](#lazy-natural-numbers)
  - [Week5\&6](#week56)
    - [User defined data types - part 2](#user-defined-data-types---part-2)
      - [Binary search trees](#binary-search-trees)
      - [Different Trees](#different-trees)
      - [Permutation trees, list permutations, and paths in such trees (hard)](#permutation-trees-list-permutations-and-paths-in-such-trees-hard)
      - [Expression trees](#expression-trees)
      - [Types with a single constructor](#types-with-a-single-constructor)
  - [Usable Functions](#usable-functions)

# Note of FP

## Week 1

### Types in Programming, types in Haskell

#### Why are types useful?
```hs
-- 布尔值-Bool
Prelude> :type False
False :: Bool

Prelude> :type True
True :: Bool

-- 函数-(..)
Prelude> :type not
not :: Bool -> Bool
```

#### Type inference before evaluation

```hs
-- 类型检查
Prelude> :type (!!)
(!!) :: [a] -> Int -> a

-- 类型正确但表达式不一定正确
Prelude> :type ["foo", "bar"] !! 5
["foo", "bar"] !! 5 :: [Char]

Prelude> ["foo", "bar"] !! 5
"*** Exception: Prelude.!!: index too large

Prelude> :type 1 `div` 0
1 `div` 0 :: Integral a => a

Prelude> 1 `div` 0
*** Exception: divide by zero
```

#### Tour of Haskell types

```hs
-- 布尔值-Bool
Prelude> :type False
False :: Bool

-- 字符-Char
Prelude> :type 'c'
'c' :: Char

-- 字符串-[Char]
Prelude> :type "foo"
"foo" :: [Char]

-- 整数-Int(固定精度)
Prelude> :type 2 :: Int
2 :: Int :: Int

-- 整数-Integer(非固定精度)
Prelude> :type 2 :: Integer
2 :: Integer :: Integer

-- 浮点数-Float
Prelude> sqrt 2 :: Float
1.4142135

-- 双精度浮点数-Double
Prelude> sqrt 2 :: Double
1.4142135623730951
```
#### A useful `ghci` command: `:info`

```hs
Prelude> :info Bool
data Bool = False | True 	-- Defined in ‘GHC.Types’
instance Eq Bool -- Defined in ‘GHC.Classes’
instance Ord Bool -- Defined in ‘GHC.Classes’
instance Show Bool -- Defined in ‘GHC.Show’
instance Read Bool -- Defined in ‘GHC.Read’
instance Enum Bool -- Defined in ‘GHC.Enum’
instance Bounded Bool -- Defined in ‘GHC.Enum’

Prelude> :info not
not :: Bool -> Bool 	-- Defined in ‘GHC.Classes’
```

### Polymorphism

A function whose type contains one (or several) variables is called polymorphic.
The functions [] and (:) are examples of polymorphic functions.

类型包含一个（或多个）变量的函数称为多态函数。
函数 [] 和 (:) 是多态函数的示例。

```hs
Prelude> :t zip
zip :: [a] -> [b] -> [(a, b)]
```

## Week 2

### Type classes and instances

#### The type class `Eq`
```hs
- Eq约束了a的类型, a类型都是一样的
Prelude> :type (==)
(==) :: Eq a => a -> a -> Bool

Prelude> :info Eq
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
  	-- Defined in ‘GHC.Classes’
instance (Eq a, Eq b) => Eq (Either a b)
  -- Defined in ‘Data.Either’
instance Eq a => Eq [a] -- Defined in ‘GHC.Classes’
instance Eq Word -- Defined in ‘GHC.Classes’
instance Eq Ordering -- Defined in ‘GHC.Classes’
instance Eq Int -- Defined in ‘GHC.Classes’
instance Eq Float -- Defined in ‘GHC.Classes’
instance Eq Double -- Defined in ‘GHC.Classes’
instance Eq Char -- Defined in ‘GHC.Classes’
instance Eq Bool -- Defined in ‘GHC.Classes’
...
instance (Eq a, Eq b, Eq c) => Eq (a, b, c)
  -- Defined in ‘GHC.Classes’
instance (Eq a, Eq b) => Eq (a, b) -- Defined in ‘GHC.Classes’
instance Eq () -- Defined in ‘GHC.Classes’
instance Eq Integer
  -- Defined in ‘integer-gmp-1.0.2.0:GHC.Integer.Type’
instance Eq a => Eq (Maybe a) -- Defined in ‘GHC.Base’
```

### Functions in Haskell

#### Composing functions

```hs
-- 在函数当中引用另一个函数(函数的套用)
removeLast :: [a] -> [a]
removeLast xs = reverse (tail (reverse xs))

removeElem :: Int -> [a] -> [a]
removeElem n xs = removeLast (take n xs) ++ drop n xs
```
#### Conditionals

```hs
-- 函数中的条件语句
howMuchDoYouLikeHaskell :: Int -> String
howMuchDoYouLikeHaskell x = if x < 3 then "I dislike it!" else
                               if x < 7 then "It's ok!" else
                                 "It's fun!"
```

#### Guarded equations

```hs
-- 类似于条件语句的保护方程
abs :: Int -> Int
abs n | n >= 0    = n
      | otherwise = -n
```

#### Pattern matching

```hs
-- 模式配对(符合则进行..)
isEmpty :: [a] -> Bool
isEmpty []     = True
isEmpty (x:xs) = False

-- 可能存在未穷尽情况(会报错)
isTrue :: Bool -> Bool
isTrue True = True
*Main> isTrue False
*** Exception: defining-functions.hs:36:1-18: Non-exhaustive patterns in function isTrue

-- 元组的引用和省略(不可更改的数组)
third :: (a, b, c) -> c
third (_, _, z) = z

-- 列表的引用和省略(可以更改的数组)
isEmpty' :: [a] -> Bool
isEmpty' [] = True
isEmpty' (x:xs) = False

-- case of的表达
isEmpty2 :: [a] -> Bool
isEmpty2 x = case x of [] -> True
                       (_:_) -> False
```

#### Lambda expressions

```hs
-- lambda表达式的引用
mult :: Int -> Int -> Int
mult x y = x * y

mult' :: Int -> Int -> Int
mult' = \x y -> x * y

-- lambda作为函数的引用
*Main> apply (\_ -> 5) 'r'
5
```

#### Operators and sections

```hs
-- 运算符的引用
square :: Int -> Int
square = (^2)

reci :: Fractional a => a -> a
reci = (1 /)
```

### More on type classes and instances

#### The type `Ordering` and the typeclass `Ord`

```hs
-- Ord实现了比较相等甚至比较大小
data  Ordering  =  LT | EQ | GT
          deriving (Eq, Ord, Enum, Read, Show, Bounded)

class  (Eq a) => Ord a  where
    compare              :: a -> a -> Ordering
    (<), (<=), (>=), (>) :: a -> a -> Bool
    max, min             :: a -> a -> a

        -- Minimal complete definition:
        --      (<=) or compare
        -- Using compare can be more efficient for complex types.
    compare x y
         | x == y    =  EQ
         | x <= y    =  LT
         | otherwise =  GT

    x <= y           =  compare x y /= GT
    x <  y           =  compare x y == LT
    x >= y           =  compare x y /= LT
    x >  y           =  compare x y == GT

-- note that (min x y, max x y) = (x,y) or (y,x)
    max x y
         | x <= y    =  y
         | otherwise =  x
    min x y
         | x <= y    =  x
         | otherwise =  y
```

### Type clases in more detail

#### Motivation

```hs
-- == 并不能比较函数 但是可以通过添加实例增加这个功能
Prelude> f1 == f2

<interactive>:14:1: error:
• No instance for (Eq (Bool -> Bool)) arising from a use of ‘==’
        (maybe you haven't applied a function to enough arguments?)
    • In the expression: f1 == f2
      In an equation for ‘it’: it = f1 == f2

instance Eq (Bool -> Bool) where
  f == g = f True == g True && f False == g False
```

## Week 3

### List Comprehensions

```hs
-- 本章所需要的库
import Data.Char
```

#### Basic Concepts

```hs
-- 根据`|`后面的规则进行推导(遍历)
> [(x,y) | x <- [1,2,3], y <- [4,5]]
[(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]
```

#### Dependent Generators

```hs
-- 逐步细化来取sub
concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

-- 有时`_`用来丢弃
firsts :: [(a,b)] -> [a]
firsts ps = [x | (x, _) <- ps]
```

#### Guards

```hs
-- 单个遍历情况下, 添加条件函数(true则会取)
> [x | x <- [1..10], even x]
[2,4,6,8,10]
```

#### The Zip Function

```hs
-- zip函数把两个列表组合变成二维列表
> zip ['a','b','c'] [1,2,3,4]
[('a',1),('b',2),('c',3)]
```

#### String Comprehensions

```hs
-- !!取index位数
> "abcde" !! 2
'c'

-- take 取前n位
> take 3 "abcde"
"abc"

-- length 输出总长
> length "abcde"
5

-- zip 压缩两列表
> zip "abc" [1,2,3,4]
[('a',1),('b',2),('c',3)]
```

#### Extended Programming Example - The Caesar Cipher

```
-- a..z 转换为 0-25
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

-- 字母向右移n位 z后面是a(通过mod实现)
shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

-- 把shift应用到每一位
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
```

#### Frequency Tables

```hs
-- 频率转为百分比
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

-- 求每一个字母(不是每一位)的频率,存在0(即当前字符串没有这个字母)
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = lowers xs
```

#### Cracking the Cipher(硬破解,没有给出关键数)

```hs
-- 运用了一个数学公式 chi-square statistic
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

-- 字母往左移 a前面是z
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- 通过取最小频率的字母推断可能的关键数(存在错误情况)
crack :: String -> String
crack xs = encode (-factor) xs
  where
     factor = head (positions (minimum chitab) chitab)
     chitab = [chisqr (rotate n table') table | n <- [0..25]]
     table' = freqs xs
```

### Recursive Functions

#### Basic Concepts

```hs
-- product [a] 会把所有的元素相乘
fac :: Int -> Int
fac n = product [1..n]

-- fac内部调用自己
fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

-- 列表的递归
product' :: Num a => [a] -> a
product' []     = 1
product' (n:ns) = n * product' ns
```

#### Multiple Recursion

```hs
-- Fibonacci number 菲波那切数列
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)
```

#### Mutual Recursion

```hs
-- 两个函数之间互相调用() 主要用于两种结果的情况

-- 函数调用
even :: Int -> Bool
even 0 = True
even n = odd (n-1)

odd :: Int -> Bool
odd 0 = False
odd n = even (n-1)

-- 列表解法
evens :: [a] -> [a]
evens []     = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds []     = []
odds (_:xs) = evens xs
```

#### Advice on Recursion

1. Define the type

  ```hs
  drop :: Int -> [a] -> [a]
  ```

2. enumerate the cases

  ```hs
  drop 0 []     = 
  drop 0 (x:xs) = 
  drop n []     = 
  drop n (x:xs) = 
  ```

3. Define the simple cases

  ```hs
  drop 0 []     = []
  drop 0 (x:xs) = x:xs
  drop n []     = []
  drop n (x:xs) = 
  ```

4. Define the other cases

  ```hs
  drop 0 []     = []
  drop 0 (x:xs) = x:xs
  drop n []     = []
  drop n (x:xs) = drop (n-1) xs
  ```

5. generalise and simplify

  ```hs
  drop :: Int -> [a] -> [a]
  drop 0 xs     = xs
  drop _ []     = []
  drop n (_:xs) = drop (n-1) xs
  ```

### Higher-order Functions

#### Basic Concepts

引用函数作为参数, 则大函数就是高阶函数

```hs
-- 讲一个函数对一个值应用两次, 关于函数作为参数的例子
twice :: (a -> a) -> a -> a
twice f x = f (f x)
```

#### Processing Lists

- The Map Function

  ```hs
  -- map将会对列表里每一个元素进行函数应用
  map :: (a -> b) -> [a] -> [b]
  map f xs = [f x | x <- xs]

  map :: (a -> b) -> [a] -> [b]
  map f []     = []
  map f (x:xs) = f x : map f xs
  ```

- The Filter Function

  ```hs
  -- filter将对对列表里每一个元素进行筛选 把函数结果为true的筛选出来 (函数参数结果一定为Bool)
  filter :: (a -> Bool) -> [a] -> [a]
  filter p [] = []
  filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
  ```

#### The Foldr Function

```hs
-- 列表按从左往右的顺序依次堆叠
sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product []     = 1
product (x:xs) = x * product xs

and :: [Bool] -> Bool
and []     = True
and (x:xs) = x && and xs

-- foldr函数 将会向右堆叠 最后一个值将也会被算入
sum = foldr (+) 0

product = foldr (*) 1

or = foldr (||) False

and = foldr (&&) True

-- foldr函数的定义
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v []     = v
foldr f v (x:xs) = f x (foldr f v xs)
```

#### The Foldl Function

```hs
-- foldl函数 将会向左堆叠 最后一个值也会被算入
product :: Num a => [a] -> a
product = foldl (*) 1

or :: [Bool] -> Bool
or = foldl (||) False

and :: [Bool] -> Bool
and = foldl (&&) True

length :: [a] -> Int
length = foldl (\n _ -> n+1) 0

-- foldl函数的定义
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f v []     = v
foldl f v (x:xs) = foldl f (f v x) xs
```

#### The Composition Operator

主要是`.`,`map`.`filter`,`and`,`or`,`any`,`takeWhile`,`dropWhile`等的应用

## Week 4

### User defined data types - part 1

#### User defined data types

- The booleans revisited

  ```hs
  data Bool = False | True
  False :: Bool
  True  :: Bool

  (&&) :: Bool -> Bool -> Bool
  False && _ = False
  True  && x = x

  conj :: Bool -> Bool -> Bool
  conj False False = False
  conj False True  = False
  conj True  False = False
  conj True  True  = True
  ```

- Type isomorphisms

  ```hs
  -- 两者结构上是互逆的
  data BW = Black | White

  bw2bool :: BW -> Bool
  bw2bool Black = False
  bw2bool White = True

  bool2bw :: Bool -> BW
  bool2bw False = Black
  bool2bw True  = White

  bw2bool' :: BW -> Bool
  bw2bool' Black = True
  bw2bool' White = False

  bool2bw' :: Bool -> BW
  bool2bw' False = White
  bool2bw' True  = Black
  ```

- Weekdays

  ```hs
  data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun
               deriving (Show, Read, Eq, Ord, Enum)
  ```

#### Some important type constructors

- The Maybe type constructor

  ```hs
  -- 添加条件 防止意外情况
  dive :: Int -> Int -> Maybe Int
  x `dive` y = if y == 0 then Nothing else Just (x `div` y)
  ```

- The Either type constructor

  ```hs
  data Either a b = Left a | Right b
    Left  :: a -> Either a b
    Right :: b -> Either a b
  ```

- The And type constructor, defined by ourselves

  ```hs
  -- data和type之间的区别与关联

  data And a b = Both a b
    Both :: a -> b -> And a b
  
  data MainDish = Chicken | Pasta | Vegetarian
  data Dessert = Cake | IceCream | Fruit
  data Drink = Tea | Coffee | Beer

  type SaverMenu = Either (And MainDish Dessert) (And MainDish Drink)

  prime :: SaverMenu -> SaverMenu'
  prime (Left (Both m d)) = Both m (Left  d)
  prime (Right(Both m d)) = Both m (Right d)

  unprime :: SaverMenu' -> SaverMenu
  unprime (Both m (Left  d)) = Left (Both m d)
  unprime (Both m (Right d)) = Right(Both m d)

  and2pair :: And a b -> (a,b)
  and2pair (Both x y) = (x,y)

  pair2and :: (a,b) -> And a b
  pair2and (x,y) = Both x y

  type SaverMenu''  = Either (MainDish, Dessert) (MainDish, Drink)
  type SaverMenu''' = (MainDish, Either Dessert Drink)
  ```
#### Lists revisited

```hs
data [a] = [] | a : [a]

data List a = Nil | Cons a (List a)
  Nil  :: List a
  Cons :: a -> List a -> List a

nativelist2ourlist :: [a] -> List a
nativelist2ourlist []     = Nil
nativelist2ourlist (x:xs) = Cons x (nativelist2ourlist xs)

ourlist2nativelist :: List a -> [a]
ourlist2nativelist Nil         = []
ourlist2nativelist (Cons x xs) = x:ourlist2nativelist xs
```

- Implementing some basic operations on lists

  ```hs
  append :: List a -> List a -> List a
  append Nil         ys = ys
  append (Cons x xs) ys = Cons x (append xs ys)

  rev :: List a -> List a
  rev Nil         = Nil
  rev (Cons x xs) = rev xs `append` (Cons x Nil)

  -- 添加辅助空列表
  fastrev :: List a -> List a
  astrev xs = revapp xs Nil
   where
     revapp :: List a -> List a -> List a
     revapp (Cons x xs) ys = revapp xs (Cons x ys)
     revapp Nil         ys = ys

  {-
    fastrev (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))
  = revapp (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))) Nil
  = revapp (Cons 2 (Cons 3 (Cons 4 Nil))) (Cons 1 Nil)
  = revapp (Cons 3 (Cons 4 Nil)) (Cons 2 (Cons 1 Nil))
  = revapp (Cons 4 Nil) (Cons 3 (Cons 2 (Cons 1 Nil)))
  = revapp Nil (Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil))))
  = Cons 4 (Cons 3 (Cons 2 (Cons 1 Nil)))
  }
  ```

- An aside on accumulators

  ```hs
  -- 菲波那切数列数学定义的直接翻译
  fib 0 = 0
  fib 1 = 1
  fib n = fib (n-1) + fib (n-2)

  -- 优化 添加辅助参数
  fastfib n = fibAcc n 0 1
  where
    fibAcc 0 x y = x
    fibAcc 1 x y = y
    fibAcc n x y = fibAcc (n-1) y (x+y)

  {-
    fastfib 7
  = fibAcc 7 0 1
  = fibAcc 6 1 1
  = fibAcc 5 1 2
  = fibAcc 4 2 3
  = fibAcc 3 3 5
  = fibAcc 2 5 8
  = fibAcc 1 8 13
  = 13
  }
  ```

#### Binary tree

```hs
-- 定义数据结构
data BT a = Empty
          | Fork a (BT a) (BT a) deriving (Show, Read, Eq, Ord)
  Empty :: BT a
  Fork  :: a -> BT a -> BT a -> BT a

-- 镜面
mirror :: BT a -> BT a
mirror Empty = Empty
mirror (Fork x l r) = Fork x (mirror r) (mirror l)

-- 大小
size :: BT a -> Integer
size Empty        = 0
size (Fork x l r) = 1 + size l + size r

-- 树叶数量
leaves :: BT a -> Integer
leaves Empty        = 1
leaves (Fork x l r) = leaves l + leaves r

-- 高度
height :: BT a -> Integer
height Empty        = 0
height (Fork x l r) = 1 + max (height l) (height r)
```

- Directions, addresses and paths in binary trees

  ```hs
  -- 用到了maybe
  data Direction = L | R deriving (Show)
  type Address   = [Direction]

  subtree :: Address -> BT a -> Maybe(BT a)
  subtree []     t            = Just t
  subtree (_:_)  Empty        = Nothing
  subtree (L:ds) (Fork _ l _) = subtree ds l
  subtree (R:ds) (Fork _ _ r) = subtree ds r

  isValid :: Address -> BT a -> Bool
  isValid []     _            = True
  isValid (_:_)  Empty        = False
  isValid (L:ds) (Fork _ l _) = isValid ds l
  isValid (R:ds) (Fork _ _ r) = isValid ds r

  validAddresses :: BT a -> [Address]
  validAddresses Empty        = [[]]
  validAddresses (Fork _ l r) = [[]]
                             ++ [L:ds | ds <- validAddresses l]
                             ++ [R:ds | ds <- validAddresses r]

  validAddresses' :: BT a -> [Address]
  validAddresses' Empty        = [[]]
  validAddresses' (Fork _ l r) = [[]]
                              ++ (map (L:) (validAddresses' l))
                              ++ (map (R:) (validAddresses' r))

  btpaths :: BT a -> [[a]]
  btpaths Empty        = [[]]
  btpaths (Fork x l r) = [x:xs | xs <- btpaths l]
                      ++ [x:xs | xs <- btpaths r]
  ```

- Traversals in binary trees

  ```hs
  treeInOrder :: BT a -> [a]
  treeInOrder Empty = []
  treeInOrder (Fork x l r) = treeInOrder l ++ [x] ++ treeInOrder r

  treePreOrder :: BT a -> [a]
  treePreOrder Empty = []
  treePreOrder (Fork x l r) = [x] ++ treePreOrder l ++ treePreOrder r

  levels :: BT a -> [[a]]
  levels Empty        = []
  levels (Fork x l r) = [[x]] ++ zipappend (levels l) (levels r)
    where
      zipappend []       yss      = yss
      zipappend xss      []       = xss
      zipappend (xs:xss) (ys:yss) = (xs ++ ys) : zipappend xss yss

  treeBreadthFirst :: BT a -> [a]
  treeBreadthFirst = concat . levels
  ```

- Inverting traversals (generating trees)

  ```hs
  treeInOrder, treePreOrder, treeBreadthFirst :: BT a -> [a]

  balancedTree :: [a] -> BT a
  balancedTree [] = Empty
  balancedTree xs = let (ys, x:zs) = splitAt (length xs `div` 2) xs in
                    Fork x (balancedTree ys) (balancedTree zs)

  balance :: BT a -> BT a
  balance = balancedTree . treeInOrder

  inOrderTree :: [a] -> [BT a]
  inOrderTree [] = [Empty]
  inOrderTree xs = [Fork x l r | i <- [0..length xs-1],
                                 let (ys, x:zs) = splitAt i xs,
                                 l <- inOrderTree ys, r <- inOrderTree zs]
  ```

### Lazy natural numbers

```hs
-- 列表很大或者无限时 无法给出答案
checkLengthBiggerThan :: [a] -> Int -> Bool
checkLengthBiggerThan xs n = length xs > n

-- 因为n的值会变化 所以会更加快速的得出答案
checkLengthBiggerThan' :: [a] -> Int -> Bool
checkLengthBiggerThan' []     0 = False
checkLengthBiggerThan' xs     0 = True
checkLengthBiggerThan' []     n = False
checkLengthBiggerThan' (x:xs) n = checkLengthBiggerThan' xs (n-1)
```

```hs
-- 基础的定义
data Nat = Zero | Succ Nat deriving (Eq,Ord)

-- 特例的展示
one, two, three :: Nat
one   = Succ Zero
two   = Succ one
three = Succ two

-- 自制变量的转换
toNat :: Int -> Nat
toNat 0 = Zero
toNat n = Succ (toNat (n-1))

-- 基于自制函数的长度函数
length' :: [a] -> Nat
length' []     = Zero
length' (x:xs) = Succ (length' xs)

-- 基于自制函数的大小比较
biggerThan :: Nat -> Nat -> Bool
Zero     `biggerThan` y        = False
(Succ x) `biggerThan` Zero     = True
(Succ x) `biggerThan` (Succ y) = x `biggerThan` y

checkLengthBiggerThan'' :: [a] -> Int -> Bool
checkLengthBiggerThan'' xs n = (length' xs) `biggerThan` (toNat n)

-- 如果派生了ord 可以直接用 > 而不用自制函数
checkLengthBiggerThan''' :: [a] -> Int -> Bool
checkLengthBiggerThan''' xs n = length' xs > toNat n
```

## Week5&6

### User defined data types - part 2

#### Binary search trees

```hs
-- pre-conditions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import Data1
import System.Random
```

```hs
-- 左遍历全小于, 右遍历全大于, 检查左右子树是否是正确格式
isBST :: Ord a => BT a -> Bool
isBST Empty        = True
isBST (Fork x l r) = allSmaller x l
                  && allBigger  x r
                  && isBST l
                  && isBST r

allSmaller :: Ord a => a -> BT a -> Bool
allSmaller x Empty        = True
allSmaller x (Fork y l r) = y < x
                         && allSmaller x l
                         && allSmaller x r

allBigger :: Ord a => a -> BT a -> Bool
allBigger x Empty = True
allBigger x (Fork y l r) = y > x
            && allBigger x l
            && allBigger x r
```

```hs
-- BST转化为列表后进行排序核对
isBST' :: Ord a => BT a -> Bool
isBST' t = isIncreasing(treeInOrder t)

isIncreasing :: Ord a => [a] -> Bool
isIncreasing []       = True
isIncreasing (x:[])   = True
isIncreasing (x:y:zs) = x < y && isIncreasing(y:zs)
```

```hs
occurs :: Ord a => a -> BT a -> Bool
occurs x Empty        = False
occurs x (Fork y l r) = x == y
                     || (x < y && occurs x l)
                     || (x > y && occurs x r)

insert :: Ord a => a -> BT a -> BT a
insert x Empty                    = Fork x Empty Empty
insert x (Fork y l r) | x < y     = Fork y (insert x l) r
                      | x > y     = Fork y l (insert x r)
                      | otherwise = Fork y l r

insert' :: Ord a => a -> BT a -> Maybe(BT a)
insert' x Empty                   = Just(Fork x Empty Empty)
insert' x (Fork y l r) | x < y     = case insert' x l of
                                       Nothing -> Nothing
                                       Just l' -> Just(Fork y l' r)
                       | x > y     = case insert' x r of
                                       Nothing -> Nothing
                                       Just r' -> Just(Fork y l r')
                       | otherwise = Nothing
```

```hs
-- 删除后找到最大的作为替代,非最大的作为子树
delete :: Ord a => a -> BT a -> BT a
delete x Empty = Empty -- or you may prefer undefined (and even Nothing)
delete x (Fork y l r) | x < y                = Fork y (delete x l) r
                      | x > y                = Fork y l (delete x r)
                      | x == y && l == Empty = r
                      | x == y && r == Empty = l
                      | otherwise            = Fork (largestOf l) (withoutLargest l) r

largestOf :: Ord a => BT a -> a
largestOf Empty            = undefined
largestOf (Fork x l Empty) = x
largestOf (Fork x l r)     = largestOf r

withoutLargest :: Ord a => BT a -> BT a
withoutLargest Empty            = undefined
withoutLargest (Fork x l Empty) = l
withoutLargest (Fork x l r)     = Fork x l (withoutLargest r)
```

Testing Part

```hs
randomInts :: [Int]
randomInts = randomRs (minBound,maxBound) (mkStdGen seed)
             where seed = 42

inserts :: Ord a => [a] -> BT a -> BT a
inserts []     t = t
inserts (x:xs) t = inserts xs (insert x t)

aBigBST :: BT Int
aBigBST = inserts (take (10^6) randomInts) Empty

itsHeight = height aBigBST
itsSize   = size aBigBST
itsBST    = isBST aBigBST
itsBST'   = isBST' aBigBST

-- delete的复数操作
deletes :: Ord a => [a] -> BT a -> BT a
deletes []     t = t
deletes (x:xs) t = deletes xs (delete x t)

aSmallerTree :: BT Int
aSmallerTree = deletes (take (5 * (10^5)) randomInts) aBigBST

evenBigger :: BT Int
evenBigger = inserts (take (10^7) randomInts) Empty
```

```hs
--增加堆栈的大小
$  ghci data.hs +RTS -K1G -RTS
```

```hs
fullBST :: Integer -> Integer -> BT Integer
fullBST x y | x == y    = Fork x Empty Empty
            | x+1 == y  = Fork y (Fork x Empty Empty) Empty
            | x+1 <  y  = Fork m (fullBST x (m-1)) (fullBST (m+1) y)
            | otherwise = undefined
  where m = (x + y) `div` 2
```

BST排序, 快速排序, 归并排序

```hs
bstsort :: Ord a => [a] -> [a]
bstsort xs = treeInOrder(inserts xs Empty)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [l | l <- xs, l < x]
            ++ [x]
            ++ qsort [r | r <- xs, r >= x]

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

eosplit :: [a] -> ([a],[a])
eosplit []       = ([],[])
eosplit [x]      = ([x],[])
eosplit (e:o:xs) = case eosplit xs of
                     (es,os) -> (e:es, o:os)

msort :: Ord a => [a] -> [a]
msort xs | length xs <= 1  =  xs
         | otherwise       = merge (msort es) (msort os)
                             where (es, os) = eosplit xs
```

#### Different Trees

```hs
-- Rose tree

data Rose a = Branch a [Rose a]

rsize :: Rose a -> Integer
rsize (Branch _ ts) = 1 + sum [rsize t | t <- ts]

rsize' :: Rose a -> Integer
rsize' (Branch _ ts) = 1 + sum (map rsize' ts)

rheight :: Rose a -> Integer
rheight (Branch _ ts) = 1 + maximum [rheight t | t <- ts] -- wrong

rheight :: Rose a -> Integer
rheight (Branch _ []) = 0
rheight (Branch _ ts) = 1 + maximum [rheight t | t <- ts]
```

```hs
-- Game trees
data GameTree board move = Node board [(move, GameTree board move)] deriving (Show)

gameTree :: (board -> [(move,board)]) -> board -> GameTree board move
gameTree plays board = Node board [(m, gameTree plays b) | (m,b) <- plays board]

type NimBoard = [Integer]
data NimMove = Remove Int Integer  deriving (Show,Eq)

nimPlays :: NimBoard -> [(NimMove,NimBoard)]
nimPlays heaps = [(Remove i k, (hs ++ h-k : hs'))
                 | i <- [0..length heaps-1],
                   let (hs, h:hs') = splitAt i heaps,
                   k <- [1..h]]

nim :: [Integer] -> GameTree NimBoard NimMove
nim = gameTree nimPlays

isWinning, isLosing :: Bool -> GameTree board move -> Bool
isWinning isMisere (Node b mgs)
        | null mgs  = isMisere
        | otherwise = any (isLosing isMisere)  [g | (m,g) <- mgs]
isLosing  isMisere (Node b mgs)
        | null mgs  = not (isMisere)
        | otherwise = all (isWinning isMisere) [g | (m,g) <- mgs]
```

#### Permutation trees, list permutations, and paths in such trees (hard)

```hs
data Tree a = EBranch [(a, Tree a)] deriving (Show)

fullPaths :: Tree a -> [[a]]
fullPaths (EBranch []) = [[]]
fullPaths (EBranch forest) = [x:p | (x,t) <- forest, p <- fullPaths t]

paths :: Tree a -> [[a]]
paths (EBranch forest) =  [] : [x:p | (x,t) <- forest, p <- paths t]

permTree :: Eq a => [a] -> Tree a
permTree xs = EBranch [ (x, permTree(xs \\\ x)) | x <- xs]
  where
    (\\\) :: Eq a => [a] -> a -> [a]
    []     \\\ _   = undefined
    (x:xs) \\\ y
      | x == y     = xs
      | otherwise  = x : (xs \\\ y)

permutations :: Eq a => [a] -> [[a]]
permutations = fullPaths . permTree

factorial n = length(permutations [1..n])

removals, removals2 :: [a] -> [(a,[a])]
removals [] = []
removals (x:xs) = (x,xs) : map (\(y,ys) -> (y,x:ys)) (removals xs)

-- Another representation of list
type DList a = [a] -> [a]

removals' :: DList a -> [a] -> [(a,[a])]
removals' f [] = []
removals' f (x:xs) = (x, f xs) : removals' (f.(x:)) xs

removals2 = removals' (\xs -> xs)

permTree2 :: [a] -> Tree a
permTree2 xs = EBranch [(y, permTree2 ys) | (y,ys) <- removals2 xs]

permutations2 :: [a] -> [[a]]
permutations2 = fullPaths . permTree2
```

#### Expression trees

```hs
data Expr a = Value a
            | FromInteger Integer
            | Negate (Expr a)
            | Abs (Expr a)
            | SigNum (Expr a)
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)

class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a

eval :: Num a => Expr a -> a
eval (Value x)       = x
eval (FromInteger n) = fromInteger n
eval (Negate e)      = negate (eval e)
eval (Abs e)         = abs(eval e)
eval (SigNum e)      = signum(eval e)
eval (Add e e')      = eval e + eval e'
eval (Mul e e')      = eval e * eval e'

class Show a where
  show :: a -> String

instance Show a => Show(Expr a) where
  show (Value x)       = show x
  show (FromInteger n) = "fromInteger(" ++ show n ++ ")"
  show (Negate e)      = "negate(" ++ show e  ++ ")"
  show (Abs e)         = "abs(" ++ show e ++ ")"
  show (SigNum e)      = "signum(" ++ show e ++ ")"
  show (Add e e')      = "(" ++ show e ++ "+" ++ show e' ++ ")"
  show (Mul e e')      = "(" ++ show e ++ "*" ++ show e' ++ ")"
```

#### Types with a single constructor

```hs
data Point = Pt Float Float

pointx, pointy :: Point -> Float
pointx (Pt x _) = x
pointy (Pt _ y) = y

data Point = Pt {pointx, pointy :: Float}

norm (Pt {pointx = x, pointy = y}) = sqrt (x*x+y*y).
```

## Usable Functions

```
:set +s -- ask ghci to print time and space usage

//p应用于列表里每一个元素
map p []

//取出每一个符合p的元素
filter p []

//取出符合p的索引值
findIndices p []

//从列表里取
take n []//take 10 (iterate (2*) 1)

//一直取直到达到目标的列表（用来算次数）
takeWhile (<p) (iterate p0 a)

//列表递推
scanl f n [](依据f 每次取两个值)

//排序
sort []/""

//有条件的排序(涉及LT EQ GT)
sortBy compare []

//两元素之间插入
intercalate "+" []

//合并元素
concat ['a','b'] = ["ab"]

//合并并且map
concatMap f []

//列表求和
sum []

//是否全部符合条件
all f []

//列表乘积
product []

//列表 和
and []

//列表 或
or []

//列表添加开头
a:[]

//列表合并
[]++[]

//列表无限循环
cycle

//转fractional
toRational a

//字符串分开String->[String]
words []//splitOn " "(可以处理多个空格)

//字符串合并[String] -> String
unwords []

//string之间添加'\n'
unlines ["aa","bb","cc","dd","ee"] -> "aa\nbb\ncc\ndd\nee\n"

//列表长度(int结果)/列表长度(Integral结果)
length []/genericLength []

//列表翻转
reverse []

//删除索引
delete x xs

//删除对应元素
xs // ys

//列表头/尾
head/tail []

//在某一处(index)断开
splitAt 1 [1,5,6] -> ([1],[5,6])

//从int转成其他
formIntegral a

//int转digit(char)
intToDigit a

//判断是否有digit
isDigit '0' -> True

//字符转int/字符串转int list/读取(可以处理负数)
digitToInt ""/map digitToInt ""/read

//读取十六进制（Numeric）
readHex

//int->string
show a

//最小公倍数
lcm 4 8 -> 8

//最大公差数
gcd 12 8 -> 4

//小数转string
printf "%.f" f

//相反数
negate n

//列表中的数
startsWithOneOf []

//列表最大值
maximum []

//两个数最大值
max a b

//有规律的max
maximumBy (on compare $ a) []

//从列表删除符合条件的/处理后再删除
dropWhile p []/dropWhile p1 $ p2 <$> []

//同时计算余数跟除数
quotRem a b -> (商,余)

//取一个数的平方根(结果为float)
sqrt a

//向下取整
floor a

//向上取整
ceiling a

//元素索引值(带just)
elemIndex a []

//检查是否含有某元素
elem a [] -> Bool

//去掉just
fromJust

//向左/右堆叠
foldl/foldr p []

//zip压缩
zip [] [] -> [()]

//zipWith 两列表处理
zipWith p [] [] // zipWith max [] []取出每一位最大的数

//unzip
unzip [()] -> ([])

//uncurry去除括号并计算
uncurry mod (5,4) -> 1

//去除括号！！！
\(x,y) -> x-y

//取左或者右
fst (1,2)->1
snd (1,2)->2

//取括号不同位置的值
snd x // fst x

//nub去除重复
nub [1,1,2,3,4,1] -> [1,2,3,4]

//翻转参数
flip p a b -> p b a

//判断是否字母(全)
isLetter c && isAscii c

//ord字母转ascii
ord 'a' = 97

//chr ascii转字母
chr 97 = 'a'

//either二选一
either read id

// 输出 打印
putStrLn ""

// 输出日志
runWriter (f n) (f里面有tell [])

// 输出计数
runState (f n) a (f里面有 modify f' (对a进行处理))

//分别执行f
[f1,f2] <*> [1..5] 类似于map但是是多个

// 调用函数
xs >>= f

// map的方程应用
fmap f/ liftM

//不满足左则执行右
item <|> return 'd'

//连接列表 m (m a) -> m a
join [[][]] -> []

//过滤 
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]  

//折叠
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a  

//many 0或者多个
parse (many digit) "123abc" -> [("123","abc")]

//some 至少一个
parse (some digit) "abc" -> []

//执行后面的函数
a >>= f

//一个数的除数
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

//是否为sqrt
isSquare :: Integral n => n -> Bool
isSquare n
    | n < 0 = False
    | otherwise = root^2 == n
    where root = floor (sqrt (fromIntegral n))
或者
(round . sqrt $ fromIntegral x)^2 == x

//是否为质数
isprime :: Int -> Bool
isprime c = [x | x <- [1..c], c `mod` x == 0] == [1,c]

//为import Data.List的情况下排序
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [l | l <- xs, l < x]
            ++ [x]
            ++ qsort [r | r <- xs, r >= x]

//菲波那切数列
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

//除法非0报错
dive :: Int -> Int -> Maybe Int
x `dive` y = if y == 0 then Nothing else Just (x `div` y)

//第一个元素位置
firstPosition :: Eq a => a -> [a] -> Maybe Int
firstPosition x []     = Nothing
firstPosition x (y:ys)
           | x == y    = Just 0
           | otherwise = case firstPosition x ys of
                           Nothing -> Nothing
                           Just n  -> Just (n+1)

//二叉树镜像
mirror :: BT a -> BT a
mirror Empty = Empty
mirror (Fork x l r) = Fork x (mirror r) (mirror l)

//二叉树大小
size :: BT a -> Integer
size Empty        = 0
size (Fork x l r) = 1 + size l + size r

//二叉树叶子
leaves :: BT a -> Integer
leaves Empty        = 1
leaves (Fork x l r) = leaves l + leaves r

//二叉树高度
height :: BT a -> Integer
height Empty        = 0
height (Fork x l r) = 1 + max (height l) (height r)

//中序遍历
treeInOrder :: BT a -> [a]
treeInOrder Empty = []
treeInOrder (Fork x l r) = treeInOrder l ++ [x] ++ treeInOrder r

//前序遍历
treePreOrder :: BT a -> [a]
treePreOrder Empty = []
treePreOrder (Fork x l r) = [x] ++ treePreOrder l ++ treePreOrder r

//层序遍历(复杂超时)
treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels tree = reverse $ step ([],[tree])
      where step (result, trees) = case trees of
              []                            -> result
              (Nothing                :ts)  -> step (result,ts)
              ((Just (TreeNode l r v)):ts)  -> step (v:result,ts++[l,r]) 

//层序遍历(简单) - 广度优先遍历
levels :: BT a -> [[a]]
levels Empty        = []
levels (Fork x l r) = [[x]] ++ zipappend (levels l) (levels r)
  where
    zipappend []       yss      = yss
    zipappend xss      []       = xss
    zipappend (xs:xss) (ys:yss) = (xs ++ ys) : zipappend xss yss
treeBreadthFirst :: BT a -> [a]
treeBreadthFirst = concat . levels

//根据已有列表生成平衡树
balancedTree :: [a] -> BT a
balancedTree [] = Empty
balancedTree xs = let (ys, x:zs) = splitAt (length xs `div` 2) xs in
                  Fork x (balancedTree ys) (balancedTree zs)

//插入 使其保持平衡树
insertBalanced :: Tree a -> a -> Tree a
insertBalanced Leaf x = Node 1 Leaf x Leaf
insertBalanced (Node h l y r) x
    | height l > height r = Node (h + 1) l y (insertBalanced r x)
    | otherwise = Node (h + 1) (insertBalanced l x) y r
    where
     height :: Tree a -> Integer
     height Leaf = 0
     height (Node h _ _ _) = h
```