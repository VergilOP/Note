# Problem Sheet for Week 2

## Lab Video

We will begin with a short introductory [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=ea24b3c5-c0ed-4544-ae13-af2300d9503f)

## Ill-typed Expressions

1. Write five ill-typed expressions in Haskell. 

	1. `tail 3`
	1. `5 == "hello"`
	1. `'b' + 4`
	1. `toUpper 7`
	1. `if True then 6 else "seven"`
	
## Polymorphism


1. Find out the types of the following functions. Decide if they are polymorphic.

	  | Function    | Type                | Polymorphic |
	  |-------------|---------------------|-------------|
	  | `fst`       | `(a, b) -> a`       | yes         |
	  | `(++)`      | `[a] -> [a] -> [a]` | yes         |
	  | `not`       | `Bool -> Bool`      | no          |
	  | `head`      | `[a] -> a`          | yes         |
	  | `tail`      | `[a] -> [a]`        | yes         |
	  | `id`        | `a -> a`            | yes         |

2. Explain, in your own words, what the function `zip` does. In the expression `zip ['x', 'y'] [False]`, what are the type variables `a` and `b` of `zip :: [a] -> [b] -> [(a, b)]` instantiated by?

	The function zip traverses the two lists simultaneously while
	returning the pairs of the elements it encounters in a new list.
	If the lists are not the same length, the result will have the
	length of the shorter list.  In the example, we have `a = Char`
	and `b = Bool`.
	
3. Find a polymorphic function in the GHC [standard library](https://hackage.haskell.org/package/base-4.17.0.0/docs/Prelude.html) whose type contains 3 type variables or more.

	Examples are `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]` and `flip :: (a -> b -> c) -> b -> a -> c`.
	
4. Read Section 3.7 of Programming in Haskell. Compare the types of the examples given there with the types `ghci` indicates. (Note: some of the types that `ghci` shows use "type classes" - you will learn about these in the next lesson.)

	The only one which differs is `length :: Foldable t => t a -> Int`
	which is generalized from just the type `[a]` of lists to
	arbitrary members of the `Foldable` typeclass.

## Standard Library Functions and Hoogle

Look up the following functions for manipulating lists on
[Hoogle](https://hoogle.haskell.org/) and write down their types.  For
each function, read the description and try the function on a sample list.

 | Function  | Type                                     |
 |-----------|------------------------------------------|
 | length    | Foldable t => t a -> Int                 |
 | reverse   | [a] -> [a]                               |
 | tail      | HasCallStack => [a] -> [a]               |
 | head      | HasCallStack => [a] -> a                 |
 | take      | Int -> [a] -> [a]                        |
 | drop      | Int -> [a] -> [a]                        |
 | takeWhile | (a -> Bool) -> [a] -> [a]                |
 | dropWhile | (a -> Bool) -> [a] -> [a]                |
 | filter    | (a -> Bool) -> [a] -> [a]                |
 | all       | Foldable t => (a -> Bool) -> t a -> Bool |
 | any       | Foldable t => (a -> Bool) -> t a -> Bool |
 | map       | (a -> b) -> [a] -> [b]                   |

## Functions in Haskell

Here are some example functions which you can try to implement to get started using both the concepts from the lecture as well as the library functions you found above.

1. Write a function `orB :: Bool -> Bool -> Bool` that returns `True` if at least one argument is `True`.

		orB :: Bool -> Bool -> Bool
		orB True True = True
		orB True False = True
		orB False True = True 
		orB False False = False

		orB' :: Bool -> Bool -> Bool
		orB' True _ = True
		orB' _ True = True
		orB' _ _ = False
		
1. Write a function `swap :: (a, b) -> (b, a)` that swaps the elements of a pair.

		swap :: (a,b) -> (b,a)
		swap (x,y) = (y,x)
		
1. Write a function that removes both the first and the last element of a list.

		removeFirstAndLast :: [a] -> [a]
		removeFirstAndLast = reverse . tail . reverse . tail 
		
1. Write a function which returns the reverse of a list if its length is greater than 7.  Now modify the function so that the cutoff length is a parameter.

		reverse7 :: [a] -> [a]
		reverse7 l = if (length l > 7) then reverse l else l 

		reverse7guard :: [a] -> [a]
		reverse7guard l | length l > 7 = reverse l 
                        | otherwise    = l 
                
        reverseParam :: Int -> [a] -> [a]
		reverseParam n l | length l > n = reverse l
                         | otherwise    = l 
						 
1. Write a function which doubles all the elements of a list `l :: [Int]` and then keeps only those which are greater than 10. 

        doubleGtTen :: [Int] -> [Int]
        doubleGtTen l = filter (> 10) $ map (\x -> x + x) l 
		
1. Write a function to return the reverse of a string with all its alphabetic elements capitalized. (The function `toUpper :: Char -> Char` in the `Data.Char` library may be useful here.)

		revToUpper :: String -> String 
		revToUpper s = reverse $ map toUpper s

# Homework for Week 2

This homework will not be assessed.  You must complete it, however, in order to prepare for the tests.

## Writing More Functions

1. Write a function to pair each element of a list with its index.

		withIndex :: [a] -> [(Int,a)]
		withIndex l = zip [0 .. length l - 1] l 

		withIndex' :: [a] -> [(Int,a)]
		withIndex' l = [ (i, l !! i) | i <- [0 .. length l-1] ]
		
1. Using guarded equations, write a function of type `Int -> Int -> Bool` that returns `True` if the first argument is greater than the second and less than twice the second.

		betweenNand2n :: Int -> Int -> Bool
		betweenNand2n k n | k > n && k < 2 * n = True
                          | otherwise          = False 
						  
1. (Adapted and expanded from the book "Programming in Haskell)
   Define three variants of a function `third :: [a] -> a` that returns the third element in any list that contains at least this many elements, using
   
    1. `head` and `tail`
	    ```haskell
		thirdHeadTail :: [a] -> a
		thirdHeadTail = head . tail . tail 
		```
		
    2. list indexing `!!`
	
		```haskell
		thirdIndex :: [a] -> a
		thirdIndex l = l !! 2 
		``` 
	
    3. pattern matching
	
		```haskell
		thirdMatching :: [a] -> a
		thirdMatching (_:_:x:_) = x 
		```

1. (Adapted and expanded from the book "Programming in Haskell)
   Define a function `safetail :: [a] -> [a]` that behaves like tail except that it maps `[]` to `[]` (instead of throwing an error). Using `tail` and `isEmpty :: [a] -> Bool`,
   define `safetail` using
   
   1. a conditional expression
   
	   ```haskell
	   isEmpty :: [a] -> Bool
	   isEmpty [] = True
	   isEmpty _  = False
	   
	   safeTailCond :: [a] -> [a] 
	   safeTailCond l = if isEmpty l then [] else tail l 
	   ```
   2. guarded equations
   
	   ```haskell
	   safeTailGuard :: [a] -> [a]
	   sageTailGuard l | isEmpty l = []
	                   | otherwise = tail l 
	   ```
   
   3. pattern matching
   
	   ```haskell
	   safeTailMatch :: [a] -> [a]
	   safeTailMatch [] = []
	   safeTailMatch (_:xs) = xs
	   ```

## Type Classes and Instances 

1. Run, and understand, the following examples:

	| Expression                          | Result  | Explanation                                                           |
	|-------------------------------------|---------|-----------------------------------------------------------------------|
	| `False == 'c'`                      | Error   | The left side has type `Bool` while the right has type `Char`         |
	| `False == True`                     | `False` |                                                                       |
	| `False == not`                      | Error   | The left side had type `Bool` while the right has type `Bool -> Bool` |
	| `False == not True`                 | `True`  |                                                                       |
	| `not == id`                         | Error   | No instance of `Eq` for `Bool -> Bool`                                |
	| `[not] == [ (id :: Bool -> Bool) ]` | Error   | No instance of `Eq` for `Bool -> Bool`                                |
	
1. Find all the basic instances of the type class `Bounded` that are defined in the GHC Prelude (the libraries that are loaded when starting `ghci`, without importing any additional libraries). Find out what `minBound` and `maxBound` are for each of the instances.

    | Type       | minBound               | maxBound               |
    |------------|------------------------|------------------------|
    | `Word`     | `0`                    | `18446744073709551615` |
    | `Ordering` | `LT`                   | `GT`                   |
    | `Int`      | `-9223372036854775808` | `9223372036854775807`  |
    | `Char`     | `\NUL`                 | `'\1114111'`           |
    | `Bool`     | `False`                | `True`                 |

2. What type classes do the type classes `Fractional`, `Floating`, `Integral` extend? What functions do they provide? Which type class would you choose to implement a trigonometric calculus?

    | Typeclass    | Extends            | Provides                        |
    |--------------|--------------------|---------------------------------|
    | `Fractional` | `Num`              | `(/) :: a -> a -> a`            |
    |              |                    | `recip :: a -> a`               |
    |              |                    | `fromRational :: Rational -> a` |
    | `Floating`   | `Fractional`       | `pi :: a`                       |
    |              |                    | `exp :: a -> a`                 |
    |              |                    | `log :: a -> a`                 |
    |              |                    | ...                             |
    | `Integral`   | `Real a`, `Enum a` | `quot :: a -> a -> a`           |
    |              |                    | `rem :: a -> a -> a`            |
    |              |                    | `div :: a -> a -> a`            |
    |              |                    | ...                             |
