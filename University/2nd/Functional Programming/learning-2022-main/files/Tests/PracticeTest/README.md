# Practice Test

## Marking table

The exercises are defined so that it is hard to get a first-class mark.

```
  1st          - 35 marks and above.
  upper second - 30-34 marks.
  lower second - 25-29 marks.
  third        - 20-24 marks.
  fail         -  0-19 marks.
```

All questions have equal weight.

## Preparation

* The test must be completed on Jupyter Lab.
* Run `git pull` on Jupyter Lab to make sure you have the latest version of the course repository.
* Do __not__ modify either the file `Types.hs` or the file `PracticeTest-Template.hs`.
* Copy the file `PracticeTest-Template.hs` to a new file called `PracticeTest.hs` and write your solutions in `PracticeTest.hs`.

  __Don't change the header of this file, including the module declaration, and, moreover, don't change the
type signature of any of the given functions for you to complete.__

  __If you do make changes, then we will not be able to mark your submission and hence it will receive zero marks!__
* Solve the exercises below in the file `PracticeTest.hs`.

## Submission procedure

* If your submission doesn't compile or fails to pass the presubmit script on Jupyter Lab, it will get zero marks.
* Run the presubmit script provided to you on your submission from Jupyter by running `./presubmit.sh PracticeTest` in the terminal (in the same folder as your submission).
* This will check that your submission is in the correct format.
* If it is, submit on Canvas.
* Otherwise fix and repeat the presubmission procedure.

## Plagiarism

Plagiarism will not be tolerated. Copying and contract cheating have led to full loss of marks, and even module or degree failure, in the past.

You will need to sign a declaration on Canvas, before submission, that you understand the [rules](/README.md#plagiarism) and are abiding by them, in order for your submission to qualify.

## Background material

- Each question has some **Background Material**, an **Implementation Task** and possibly some **Examples**.
- Read this material first, then implement the requested function.
- The corresponding type appears in the file `PracticeTest-Template.hs` (to be copied by you).
- Replace the default function implementation of `undefined` with your own function.

## More Rules

* This is an open book test.
* You may consult your own notes, the course materials, any of the recommended books or [hoogle](https://hoogle.haskell.org/).
* Feel free to write helper functions whenever convenient.
* All the exercises may be solved without importing additional modules.  Do not import any modules, as it may interfere with the marking.

## Submission Deadline

* The official submission deadline is 2pm.
* If you are provided extra time by the Welfare office then your submission deadline is 2:30pm.

## Question 1 - Checksum

### Background Material

A *checksum* is an extra collection of bits added to some data in
order to force it to satisfy a certain property.  This property can
then be used to quickly detect simple errors which may happen during
communication.  For example, when choosing student ID numbers, we can
first choose an initial set of digits any way we please, and then add
digits to the end of the number in order to force the sum of the
digits to be divisible by 11.  By then verifying that this condition
holds whenever we process data containing the ID, we equip ourselves
with a basic sanity check.


### Implementation Task

Write a function
```haskell
checksum :: Integral a => [a] -> Bool
checksum = undefined
```
that takes as input a list of numbers and checks that

1. The list is 8 elements long
2. The sum of the numbers is divisible by 11.

The function should return `True` if both of these conditions are met, and `False` otherwise.

### Examples

```hs
*Main> checksum [8,3,2,3,8,7,0,2]
True
*Main> checksum [8,3,2,3,8,9,1,2]
False
*Main> checksum [4,5,8,2,4]
False
```

## Question 2 - Golf Scores

### Background Material

Each hole in golf has an associated "Par" -- the expected number of
Strokes it should take a good player to successfully complete the
hole.  Once they have completed the hole, a player's total number of
Strokes is compared against this Par value, and points are awarded
according to the following rules:

  - Taking two (or more) Strokes less than the Par is called an "Eagle", and is worth 4 points
  - Taking one Stroke below Par is called a "Birdie", and is worth 3 points
  - Taking the same number of Strokes as the Par is worth 2 points
  - Taking one Stroke above Par is called a "Bogey", and is worth 1 point
  - Taking any more Strokes than a "Bogey" is worth 0 points

  There is one exception to these scoring rules:
  - A "Hole-in-one" (taking exactly 1 stroke, no matter what the Par is) is always worth 5 points

### Implementation Task 

Write a function 
```haskell
golfScorer :: Integer -> Integer -> Integer
golfScorer = undefined
```
that takes a Par as the first argument and the total number of Strokes as the second argument, and retuns the amount of points awarded.

### Examples

```hs
golfScorer 3 3 = 2 -- Par (same as Par)
golfScorer 5 3 = 4 -- Eagle (2+ below Par)
golfScorer 5 2 = 4 -- Eagle (2+ below Par)
golfScorer 5 1 = 5 -- Hole-in-one
```

## Question 3 - List Comprehensions

### Background Material

In the following questions, you may wish to use the `factors` function 

```haskell
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
```

from the [lecture notes](https://git.cs.bham.ac.uk/fp/learning-2022/-/blob/main/files/LectureNotes/Sections/list_comprehensions.md#guards).

### Implementation Task 

1. List the first `n` numbers starting from 1 which are divisible by all the numbers from 2 to 12.

    ```haskell
    highlyDivisible :: Int -> [Int]
    highlyDivisible n = undefined
    ```

1. List the largest odd factor of `i` for all numbers `i` in the range `[1..n]`.

    ```haskell
    largestOddFactor :: Int -> [Int]
    largestOddFactor n = undefined
    ```
    For example, the largest odd factor of 60 is 15, since 60 = 4 * 15 and no odd number larger than 15 divides 60.


## Question 4 - Finite Types

### Background Material

Recall that it is not possible in general to have an instance of the `Eq` typeclass for function types `a -> b` for arbitrary types `a` and `b`.  On the other hand, if we make additional assumptions about `a` and `b`, sometimes it *is* possible to compare functions.

For example, let us say that a type `a` is **finite** if it is a member of both the `Enum` and `Bounded` typeclasses. The `Enum` typeclass is for those types whose elements may be *listed*, while the `Bounded` typeclass is for those types which a have a *maximal* and *minimal* element.  Recall that you can use the `:info` command in `ghci` to find out information about the names and types of the functions which are available to members of these type classes.

### Implementation Task

Write an equality comparison function

```haskell
equals :: (Bounded a, Enum a, Eq b) => (a -> b) -> (a -> b) -> Bool
equals = undefined
```
for functions whose input type is finite and whose output type supports equality testing.  Recall that two functions are considered to be equal if they return the same result for every input.

### Examples

We can consider the negation operation of 8-bit integers:
```hs
neg :: Int8 -> Int8
neg n = -n

doubleNeg :: Int8 -> Int8
doubleNeg n = - (- n)
```
For these functions, we can use our equality comparison to get
```hs
*Main> equals neg (\x -> x)
False
*Main> equals doubleNeg (\x -> x)
True
```

## Question 5 - Babylonian Palindromes

### Background Material

We say a number is a **palindrome** if it has at least two digits and appears the same when its digits are reversed.  For example `14341` is a palindrome, while `145` is not.

The notion of being a palidrome, however, is not intrinsic to a number since it depends on which *base* we use to express it (the examples above are given in base 10).  For example, the number `21` is not a palindrome in base 10, while its representation in binary (i.e., base 2) is `10101` which *is* a palindrome.

Different cultures have used different bases for their number systems throughout history.  The Babylonians, for example, wrote numbers in base 60.

### Implementation Task

Write a function 
```
babylonianPalindromes :: [Integer]
babylonianPalindromes = undefined
```
which produces the infinite list of Babylonian palindromes.
