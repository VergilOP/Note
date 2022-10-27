# Problem Sheet for Week 3

## Lab Video

We will begin with a short introductory [video](https://bham.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=a0391430-ddbf-4832-848f-af2a01288477)

## Recursive Functions

1. Without looking at the standard prelude, define the following library functions using recursion:

	* Decide if all logical values in a list are true:

		```hs
		and :: [Bool] -> Bool
		```
	* Concatenate a list of lists:

		```hs
		concat :: [[a]] -> [a]
		```
	* Produce a list with n identical elements:

		```hs
		replicate :: Int -> a -> [a]
		```

	* Select the nth element of a list:

		```hs
		(!!) :: [a] -> Int -> a
		```

	* Decide if a value is an element of a list:

		```hs
		elem :: Eq a => a -> [a] -> Bool
		```

1. Define a recursive function

	```hs
	merge :: Ord a => [a] -> [a] -> [a]
	```
	that merges two sorted lists of values to give a single sorted list. 

	For example:

	```hs
	> merge [2,5,6] [1,3,4]
	[1,2,3,4,5,6]
	```

## List Comprehensions

Please read the following handout before proceeding further [List Comprehensions](/files/LectureNotes/Sections/list_comprehensions.md)

1. A triple (x,y,z) of positive integers is called pythagorean if
x^2 + y^2 = z^2 . Using a list comprehension, define a function:

    ```hs
    pyths :: Int -> [(Int,Int,Int)]
    ```

    that maps an integer n to all such triples with components in
    [1..n]. For example:

    ```hs
    > pyths 5
    [(3,4,5),(4,3,5)]
    ```

1. A positive integer is perfect if it equals the sum of all of its
   factors, excluding the number itself. Using a list comprehension,
   define a function

    ```hs
    perfects :: Int -> [Int]
    ```

    that returns the list of all perfect numbers up to a given limit. For example:

    ```hs
    > perfects 500
    [6,28,496]
    ```

	Many variations of this exercise are possible:

    * A number which is less than the sum of its proper divisors is called [abundant](https://en.wikipedia.org/wiki/Abundant_number).
    * A number which is greater than the sum of its proper divisions is called [deficient](https://en.wikipedia.org/wiki/Deficient_number).
    * A number for which the sum of all its divisors (including itself) is greater than
	the sum of the divisors of any smaller number is called [highly abundant](https://en.wikipedia.org/wiki/Highly_abundant_number).

	For each of these variations, write a function which finds all the numbers with the
	stated property below a given number.

1. The scalar product of two lists of integers xs and ys of length n is give by the sum of the products of the corresponding integers:

    ![dot product](./assets/dot-prod.png)

    Using a list comprehension, define a function that returns the scalar product of two lists.

1.  **Harder** Implement [matrix multiplication](https://en.wikipedia.org/wiki/Matrix_multiplication) where matrices are represented by lists of lists of integers.  One possibility, for example, would be to take the dimensions of the matrices as arguments, so that your function would have type:

    ```hs
	  matrix_mul :: Int -> Int -> [[Int]] -> [[Int]] -> [[Int]]
	```

# Homework for Week 3

This homework will be marked but not counted towards your final grade. You will write your code on JupyterLab and
submit via Canvas (link below).

- Create new directory in your workspace on JupyterLab and upload/copy the files found under the `Homework3` directory.
- Copy the file `Homework3-Template.hs` to a new file called `Homework3.hs`.
- Solve the exercises below in the file `Homework3.hs`.
- Run the pre-submit script to check for any (compilation) errors **before** submitting by running in the terminal:

```bash
$ ./presubmit.sh Homework3
```
- Submit your file `Homework3.hs` via Canvas at https://canvas.bham.ac.uk/courses/65655/assignments/384995 .
- You also require the file `Types.hs` to be in the same directory as `Homework3.hs`. The file `Types.hs` should **not** be modified, and should **not** be submitted on Canvas.

0. Copy the file `Homework3-Template.hs` to a new file `Homework3.hs`.

1. We classify cars into categories according to their gas usage, measured in liters per 100 kilometers.
Consider
```haskell
data Classification = Low | Medium | High | SuperHigh deriving (Show)
```
(The `deriving (Show)` is there so that Haskell can print classifications when you're testing your program in the terminal. We will discuss this in more detail later.)

Write a function 
```haskell
gasUsage :: (Fractional a, Ord a) => a -> Classification
``` 
according to the table

| Gas Usage g        | g < 3 | 3 <= g < 5 |  5 <= g < 7 | 7 <= g    |
|--------------------|-------|------------|-------------|-----------|
| Classification     | Low   | Medium     | High        | SuperHigh |


2. (From "Programming in Haskell", Section 4.8 "Exercises", Exercise 8)

The Luhn algorithm is used to check bank card numbers for simple errors such as mistyping a digit, and proceeds as follows:
- consider each digit as a separate number;
- moving left, double every other number from the second last;
- subtract 9 from each number that is now greater than 9;
- add all the resulting numbers together;
- if the total is divisible by 10, the card number is valid.

See also the [Wikipedia entry on the Luhn algorithm](https://en.wikipedia.org/wiki/Luhn_algorithm).

Define a function
```haskell
luhnDouble :: Int -> Int
```
that doubles a digit and
subtracts 9 if the result is greater than 9. For example:
```hs
> luhnDouble 3
6
> luhnDouble 6
3
```
Using `luhnDouble` and the integer remainder function `mod`, define a function
```haskell
luhn :: Int -> Int -> Int -> Int -> Bool
```
that decides if a four digit bank card number is valid (according to the Luhn algorithm described above).
For example:
```hs
> luhn 1 7 8 4
True
> luhn 4 7 8 3
False
```

3. Run the pre-submit script to check for any (compilation) errors **before** submitting by running in the terminal:
```bash
$ ./presubmit.sh Homework3
```


