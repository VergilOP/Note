# Class Test 1 Retake

## Marking table

The exercises are defined so that it is hard to get a first-class mark.

```
  1st          - 35 marks and above.
  upper second - 30-34 marks.
  lower second - 25-29 marks.
  third        - 20-24 marks.
  fail         -  0-19 marks.
```

The module mark will be the sum of the marks on both class tests, and
your mark on this retake will *replace* your previous mark. All
questions have equal weight.

## Preparation

* The test must be completed on Jupyter Lab.
* Run `git pull` on Jupyter Lab to make sure you have the latest version of the
  course repository.
* Do __not__ modify either the file `Types.hs` or the file
  `ClassTest1Retake-Template.hs`.
* Copy the file `ClassTest1Retake-Template.hs` to a new file called `ClassTest1Retake.hs`
  and write your solutions in `ClassTest1Retake.hs`.

  __Don't change the header of this file, including the module declaration, and,
  moreover, don't change the type signature of any of the given functions for you
  to complete.__

  __If you do make changes, then we will not be able to mark your submission and
  hence it will receive zero marks!__
* Solve the exercises below in the file `ClassTest1Retake.hs`.

## Submission procedure

* If your submission doesn't compile or fails to pass the presubmit script on
  Jupyter Lab, it will get zero marks.
* Run the presubmit script provided to you on your submission from Jupyter by
  running `./presubmit.sh ClassTest1Retake` in the terminal (in the same folder as
  your submission).
* This will check that your submission is in the correct format.
* If it is, submit on Canvas.
* Otherwise fix and repeat the presubmission procedure.

## Plagiarism

Plagiarism will not be tolerated. Copying and contract cheating have led to full
loss of marks, and even module or degree failure, in the past.

You will need to sign a declaration on Canvas, before submission, that you
understand the [rules](/README.md#plagiarism) and are abiding by them, in order
for your submission to qualify.

## Background material

- Each question has some **Background Material**, an **Implementation Task** and
  possibly some **Examples**.
- Read this material first, then implement the requested function.
- The corresponding type appears in the file `ClassTest1Retake-Template.hs` (to be
  copied by you).
- Replace the default function implementation of `undefined` with your own
  function.

## More Rules

* This is an open book test.
* You may consult your own notes, the course materials, any of the recommended
  books or [Hoogle](https://hoogle.haskell.org/).
* Feel free to write helper functions whenever convenient.
* If you do write any helper function, make sure its name does not clash with
  any of the functions in the `Types.hs` file.
* All the exercises may be solved without importing additional modules. Do not
  import any modules, as it may interfere with the marking.

## Submission Deadline

* The official submission deadline is 2:25pm. Submission will be open for 10 more minutes to allow for submission difficulties, but with a penalty of 5%.
* If you are provided extra time, then your submission
  deadline is that given to you by the Welfare office.

## Question 1 [10 marks]

### Background Material

A string is said to be *periodic* of period `n` if it repeats after `n` characters.  For example,
the string
   1. `"abcabcabc"` - This string is periodic with period 3, but not periodic with period 2.
   1. `"ababa"` - This string is periodic with period 2.
   1. `"aaaaaa"` - This string is periodic with periods 1,2,3,4,5 and 6.
   1. `"abcd"` - This string is periodic with period 4.

Formally, we say that `s` is periodic if `s !! i == s !! i + n` for all `0 <= i < length s` and `n` such that `i + n < length s`

### Implementation Task

Write a function
```haskell
checkPeriodic :: String -> Int -> Bool
checkPeriodic = undefined
```
to check whether the given string is periodic with the given period.

### Examples

```hs
ghci> checkPeriodic "abcabcabc" 3
True
ghci> checkPeriodic "aaaaaa" 4
True
ghci> checkPeriodic "abca" 3
True
ghci> checkPeriodic "abcd" 3
False
```

## Question 2 [10 marks]

### Implementation Task

Write a function
```haskell
divisibleByIndex :: [Int] -> [Bool]
```
which replaces each integer in the list by the boolean which states whether that
number is divisible by its *index* in the list, with indices starting at `1` to avoid dividing by `0`.

### Examples

```hs
ghci> divisibleByIndex [3,13,9,10,25]
[True,False,True,False,True]
```

- `3` **is** divisible by `1`
- `13` **is not** divisible by `2`
- `9` **is** divisible by `3`
- and so on

## Question 3 [10 marks]

### Implementation Task

Write a function

```haskell
findCubes :: Int -> [(Int,Int,Int)]
findCubes = undefined
```
which, given a number `n`, finds all triples `(a,b,c)` of integers with `1 <= a <= b <= c` such that
```
  a^3 + b^3 + c^3 = n
```


### Examples

```hs
ghci> take 10 (filter (\n -> length (findCubes n) > 0) [1..])
[3,10,17,24,29,36,43,55,62,66]
ghci> findCubes 24
[(2,2,2)]
ghci> findCubes 55
[(1,3,3)]
```

## Question 4 [10 marks]

### Background Material

Consider the following commands for editing strings:
```haskell
data EditCommand = MoveLeft | MoveRight | Insert Char | BackSpace
```
We will be implementing these commands on a "string with a cursor".  We can represent
this situation with a pair of strings:
```haskell
type Text = (String, String)
```
The first string consists of the characters to the left of the cursor, stored backwards for convenience, and
the right string consists of the characters after the cursor.


For example, suppose we have the string `"Heelo everybody!"`, and we would like to fix the spelling mistake.  Here is what the process would look like:

| left of the cursor | right of the cursor | command |
| -- | -- | --      |
| "" | "Heelo everybody!"  | MoveRight
| "H"| "eelo everybody!"  | MoveRight
|"eH" |  "elo everybody!" | BackSpace
| "H" | "elo everybody!" | MoveRight
|"eH" |  "lo everybody!" | Insert "l"
|"leH" |  "lo everybody!" | MoveLeft
| "eH" | "llo everybody!" | MoveLeft
| "H" | "ello everybody!" | MoveLeft
| "" | "Hello everybody!" |

If a command is not possible, the text should be left unchanged, with no error given. 


### Implementation Task

Implement functions which update the current `Text`, both for single commands, as well as for a list of them, returning the final state.

```
edit :: EditCommand -> Text -> Text
edit = undefined

edits : [EditCommand] -> Text -> Text
edits = undefined
```

### Examples

```hs
ghci> edits (replicate 6 MoveRight) ("", "This is a String")
("i sihT","s a String")
ghci> edits (map Insert "Hello") ("", " World!")
("olleH"," World!")
ghci> edit MoveLeft ("","Start of Line")
("","Start of Line")
ghci> edits (replicate 9 BackSpace) (" ,eybdooG", "World!")
("","World!")
```

## Question 5 [10 marks]

### Background Material

We will think of a function `f` of type `f :: [Bool] -> Bool` as a "boolean function of multiple variables".  We say this function is *solvable in dimension `n`* if there is a list `xs :: [Bool]` of length `n` for which the value `f xs` is `True`.

### Implementation Task
Write a function `solvable` which, given a function `f :: [Bool] -> Bool` and a non-negative integer `n`, decides whether or `f` is solvable in dimension `n`.

```
solvable :: ([Bool] -> Bool) -> Int -> Bool
solvable = undefined
```

### Examples

Consider the function `f` defined by
```hs
f (a : b : c : _) = a || b || c
```
Then we have
```
solvable f 3 = True
```
Now consider the function
```hs
g (a:_) = a && not a
```
then
```
solvable g 3 = False
```
