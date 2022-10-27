# Problem Sheet for Week 4

## Lab Video

We will begin with a short introductory [video]()

## Higher Order Functions

1. Express the comprehension `[f x | x <- xs, p x]` using the functions `map` and `filter`. The function type is given as:
	```hs
	fun :: Num a => (a -> a) -> (a -> Bool) -> [a] -> [a]
	```
	For example:
	```
	> fun (^2) even [1..20]
	[4,16,36,64,100,144,196,256,324,400]

	> fun (^2) odd [1..20]
	[1,9,25,49,81,121,169,225,289,361]
	```
1. Redefine `map f` and `filter p` using `foldr`. For your reference, here are the definitions of `map` and `filter` from lecture notes.
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

1. Define a function `altMap :: (a -> b) -> (a -> b) -> [a] -> [b]` that alternatively applies the two argument functions to successive elements in a list.

	For example:
	```hs
	> altMap (+10) (+100) [0,1,2,3,4]
	[10,101,12,103,14]
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
    * Write a function to implement *multiplication* of Church numerals