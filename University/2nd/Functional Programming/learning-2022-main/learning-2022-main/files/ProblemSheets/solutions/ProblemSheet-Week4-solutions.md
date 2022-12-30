# Problem Sheet for Week 4

## Lab Video

We will begin with a short introductory [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=81a4b59a-cc63-4aad-b48b-af3101318f99)

## List comprehensions and higher-order functions

1. Express the comprehension `[f x | x <- xs, p x]` using the functions `map` and `filter`. The function type is given as:
	```haskell
    fun :: Num a => (a -> a) -> (a -> Bool) -> [a] -> [a]
    fun f p xs = map f (filter p xs)
	```
	For example:
	```
	> fun (^2) even [1..20]
	[4,16,36,64,100,144,196,256,324,400]

	> fun (^2) odd [1..20]
	[1,9,25,49,81,121,169,225,289,361]
	```
1. Redefine `map f` and `filter p` using `foldr` and `foldl`. For your reference, here are the definitions of `map` and `filter` from lecture notes. HINT. Read about the `foldr` and `foldl` functions in the handout [higher-order functions](/files/LectureNotes/Sections/higher-order_functions.md) and Chapter 7.3 and 7.4 of [Programming in Haskell](https://bham.rl.talis.com/link?url=https%3A%2F%2Fapp.kortext.com%2FShibboleth.sso%2FLogin%3FentityID%3Dhttps%253A%252F%252Fidp.bham.ac.uk%252Fshibboleth%26target%3Dhttps%253A%252F%252Fapp.kortext.com%252Fborrow%252F382335&sig=70da9a4ff905dba3523840088f10e61e90877af4795f3070b3775767fa856348).
	```hs
	map :: (a -> b) -> [a] -> [b]
	map f []     = []
	map f (x:xs) = f x : map f xs

	filter :: (a -> Bool) -> [a] -> [a]
	filter p [] = []
	filter p (x:xs)
	   | p x       = x : filter p xs
	   | otherwise = filter p xs

	```

	```haskell
    map' :: (a -> b) -> [a] -> [b]
    map' f xs = foldr (\y ys -> (f y):ys) [] xs

    map'' :: (a -> b) -> [a] -> [b]
    map'' f xs = foldl (\ys y -> ys ++ [(f y)]) [] xs


    filter' :: (a -> Bool) -> [a] -> [a]
    filter' p xs = foldr (\y ys -> if p y then y:ys else ys) [] xs

    filter'' :: (a -> Bool) -> [a] -> [a]
    filter'' p xs = foldl (\ys y -> if p y then ys . (y:) else ys) id xs []
	```

1. Define a function `altMap :: (a -> b) -> (a -> b) -> [a] -> [b]` that alternatively applies the two argument functions to successive elements in a list.

	For example:
	```hs
	> altMap (+10) (+100) [0,1,2,3,4]
	[10,101,12,103,14]
	```

	```haskell
    altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
    altMap f g [] = []
    altMap f g (x:[]) = f x : []
    altMap f g (x:y:xs) = (f x):(g y):(altMap f g xs)
	```

1. **Harder** Church Numerals

	It is possible to represent the natural numbers (i.e. 0,1,2,...) using higher
	order functions of type

	```hs
	(a -> a) -> (a -> a)
	```

	These are called **Church Numerals**. The encoding works like
	this: the input to a function of the above type is an element `f`
	of type `a -> a`.  Since such a function takes input values of
	type `a` and also produces output values of type `a`, this means
	it can be *iterated*.  So we will represent the numeral `n` by a
	the element of type `(a -> a) -> (a -> a)` which iterates its
	argument n times.

	To see the idea, the first few examples of numerals are written like
    this:

	```hs
	zero :: (a -> a) -> (a -> a)
	zero f x = x

	one :: (a -> a) -> (a -> a)
	one f x = f x

	two :: (a -> a) -> (a -> a)
	two f x = f (f x)

    three :: (a -> a) -> (a -> a)
	three f x = f (f (f x))
	```

	* Write a function to implement *addition* of Church numerals

	```haskell
    addChurch :: ((a -> a) -> (a -> a)) ->
                 ((a -> a) -> (a -> a)) ->
                 ((a -> a) -> (a -> a))
    addChurch m n f x = m f (n f x)
	```

    * Write a function to implement *multiplication* of Church numerals

	```haskell
    mulChurch :: ((a -> a) -> (a -> a)) ->
                 ((a -> a) -> (a -> a)) ->
                 ((a -> a) -> (a -> a))
    mulChurch m n f = m (n f)
	```

## Defining the prelude `concat` function in four different ways

The prelude function `concat` concatenates a list of lists, getting a single list. You will define it in four different ways, and test your implementations for correctness and efficiency.

1. Define it with comprehensions and no recursion. HINT: You will need two generators, one to extract a list xs from the list of lists xss, and another to extract an element x from the list xs, and put this in the result of the comprehension.

	```haskell
    concat1 :: [[a]] -> [a]
    concat1 xss = [x | xs <- xss, x <- xs]
	```

1. Define the same function using recursion instead. HINT. Find and use the prelude function `++`.

	```haskell
    concat2 :: [[a]] -> [a]
    concat2 [] = []
    concat2 (xs:xss) = xs ++ concat2 xss
    ```

1. Define the same function using `foldr` and `foldl`, and without recursion or list comprehensions.

	```haskell
    concat3 :: [[a]] -> [a]
    concat3 xss = foldr (++) [] xss

    concat4 :: [[a]] -> [a]
    concat4 xss = foldl (++) [] xss
	```

## Testing your `concat` functions

1. We can test the above functions as follows:

	```hs
        list = [[2,3,4],[5,6,7],[8,9,10]]
        concat1_test = concat1 list == concat list
        concat2_test = concat2 list == concat list
        concat3_test = concat3 list == concat list
        concat4_test = concat4 list == concat list
	```

   Run the above tests under `ghci`. Write also tests for the empty list. Do your functions work with the empty list?

1. Test for speed

	```hs
        biglist = replicate 1000 [1..1000]
        concat1_test2 = concat1 biglist == concat biglist
        concat2_test2 = concat2 biglist == concat biglist
        concat3_test2 = concat3 biglist == concat biglist
        concat4_test2 = concat4 biglist == concat biglist
	```

   Now run `:set +s` at the `ghci` prompt. This asks `ghci` to print time and memory usage statistics.

1. Better run-time test. Check how time increases when n increases in the following tests.

	```hs
        nlist n = replicate n [1..n]
        concat1_test3 n = concat1 (nlist n) == concat (nlist n)
        concat2_test3 n = concat2 (nlist n) == concat (nlist n)
        concat3_test3 n = concat3 (nlist n) == concat (nlist n)
        concat4_test3 n = concat4 (nlist n) == concat (nlist n)
	```

Which implementation(s) are more efficient? Some of them run in linear time and other(s) in quadratic time. Which ones?
