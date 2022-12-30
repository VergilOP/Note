# Class Test 1 

## Marking table

The exercises are defined so that it is hard to get a first-class mark.

```
  1st          - 35 marks and above.
  upper second - 30-34 marks.
  lower second - 25-29 marks.
  third        - 20-24 marks.
  fail         -  0-19 marks.
```

The module mark will be the sum of the marks on both class tests. All questions
have equal weight.

## Preparation

* The test must be completed on Jupyter Lab.
* Run `git pull` on Jupyter Lab to make sure you have the latest version of the
  course repository.
* Do __not__ modify either the file `Types.hs` or the file
  `ClassTest1-Template.hs`.
* Copy the file `ClassTest1-Template.hs` to a new file called `ClassTest1.hs`
  and write your solutions in `ClassTest1.hs`.

  __Don't change the header of this file, including the module declaration, and,
  moreover, don't change the type signature of any of the given functions for you
  to complete.__

  __If you do make changes, then we will not be able to mark your submission and
  hence it will receive zero marks!__
* Solve the exercises below in the file `ClassTest1.hs`.

## Submission procedure

* If your submission doesn't compile or fails to pass the presubmit script on
  Jupyter Lab, it will get zero marks.
* Run the presubmit script provided to you on your submission from Jupyter by
  running `./presubmit.sh ClassTest1` in the terminal (in the same folder as
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
- The corresponding type appears in the file `ClassTest1-Template.hs` (to be
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

* The official submission deadline is 2pm.
* If you are provided extra time, then your submission
  deadline is that given to you by the Welfare office.

## Question 1 [10 marks]

### Background Material

We say that a byte (a sequence of 8 bits) has even parity if the number of `1`s
is even.

### Implementation Task

Write a function

```haskell
checkParity :: String -> Bool
checkParity = undefined
```

which takes as input a string of bits and checks that 

1. the string size is a multiple of 8, and
1. each byte in the string has even parity.

The function should return `True` if both conditions are met, and `False`
otherwise.

We are representing bits here by the characters `0` and `1`.  You may
assume that the input strings contain only `0`s and `1`s.

### Examples

```hs
ghci> checkParity "01010101"
True
```
The above example has length 8 (a multiple of 8) and 4 ones (an even number).
```hs
ghci> checkParity "0111011101110110"
False
```
In the above example, the second byte has 5 ones.

```hs
ghci> checkParity "0101011"
False
```
The above example has only 7 bits (which is not a multiple of 8).

## Question 2 [10 marks]

### Background Material

A _substitution cipher_ is an old method of encryption, in which the cipher
takes a string and a _key_ that is as long as the alphabet that the message
uses. In our case, the message will be expressed using the English alphabet so
our cipher key will be a string of length 26. This represents a mapping of each
letter of the alphabet to a different letter.

For example, the key `"LYKBDOCAWITNVRHJXPUMZSGEQF"` maps `'A'` to `'L'`,
`'B'` to `'Y'`, `'C'` to `'K'` and so on.

### Implementation Task

Write a function

```haskell
substitution :: String -> String -> String
substitution plaintext key = undefined
```

which takes a plaintext string (that might contain punctuation and spaces) and
an uppercase key and returns the ciphertext.

**Note** the following:
* The capitalisation of the characters in the plaintext **must be preserved** by
  your implementation.
* The encryption should apply only to the letters (i.e. the alphabetic
  characters) and punctuation and spaces should be ignored. For this purpose,
  you can use the `isLetter :: Char -> Bool` function coming from `Data.Char` to
  test if a given character is a letter.
* You may wish to use the function 
  ```
  charLabel :: Char -> Int
  charLabel char =  ord (toUpper char) - ord 'A'
    ```
  which converts a character to an index in the key.  This function can be found in 
  `Types.hs` and will be imported for you automatically.

### Examples

```hs
key1 :: String
key1 = "LYKBDOCAWITNVRHJXPUMZSGEQF"

key2 :: String
key2 = "UDMZIQKLNJOSVETCYPBXAWRGHF"

plaintext1 :: String
plaintext1 = "The Quick Brown Fox Jumped Over The Lazy Dog"

plaintext2 :: String
plaintext2 = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

ghci> substitution plaintext1 key1
"Mad Xzwkt Yphgr Ohe Izvjdb Hsdp Mad Nlfq Bhc"

ghci> substitution plaintext1 key2
"Xli Yanmo Dptre Qtg Javciz Twip Xli Sufh Ztk"

ghci> substitution plaintext2 key1
"Nhpdv wjuzv bhnhp uwm lvdm, khrudkmdmzp lbwjwukwrc dnwm, udb bh dwzuvhb mdvjhp wrkwbwbzrm zm nlyhpd dm bhnhpd vlcrl lnwxzl. Zm drwv lb vwrwv sdrwlv, xzwu rhumpzb dedpkwmlmwhr znnlvkh nlyhpwu rwuw zm lnwxzwj de dl khvvhbh khrudxzlm. Bzwu lzmd wpzpd bhnhp wr pdjpdadrbdpwm wr shnzjmlmd sdnwm duud kwnnzv bhnhpd dz ozcwlm rznnl jlpwlmzp. Dekdjmdzp uwrm hkkldklm kzjwblmlm rhr jphwbdrm, uzrm wr kznjl xzw hoowkwl bdudpzrm vhnnwm lrwv wb dum nlyhpzv."
```

Note: these examples are provided in `Types.hs` so you can run your function on
them to test that it works correctly on them.

## Question 3 [10 marks]

### Background Material (Part 1 - [5 out of 10 marks])

A famous theorem about prime numbers (called _Chebyshev's Theorem_) asserts that
for any number `n`, there always exists a prime number `p` such that `n < p <
2n`. That is, there is always a prime number between `n` and `2n`.

#### Implementation Task

Write a function

```haskell
largestPrimeBetween :: Int -> Int
largestPrimeBetween = undefined
```

which returns the *largest* prime between `n` and `2n`.

#### Examples

```hs
ghci> largestPrimeBetween 4
7
ghci> largestPrimeBetween 10
19
```

### Background Material (Part 2 - [5 out of 10 marks])

In number theory, a **strong prime** is a prime number that is greater than the
average of the nearest prime above and below. In other words, it is closer to
the succeeding prime than it is to the preceding one.

For example, 17 is the seventh prime: the sixth and eighth primes, 13 and 19,
add up to 32, and half of that is 16; 17 is greater than 16, so 17 is a strong
prime.

#### Implementation Task 

Write a function

```haskell
strongPrimes :: Int -> [Int]
strongPrimes n = undefined
```

which takes as input the integer `n` and prints the first `n` strong prime
numbers.

### Examples

```hs
ghci> strongPrimes 25
[11,17,29,37,41,59,67,71,79,97,101,107,127,137,149,163,179,191,197,223,227,239,251,269,277]
```

## Question 4 [10 marks]

### Background Material

Consider the following data type of directions

```hs
data Direction = MoveLeft
               | MoveRight
               | MoveUp 
               | MoveDown
               deriving (Eq, Show)
```

Let us define the type `Command` to consist of a pair of a `Direction` and an
`Int`.

```hs
type Command = (Direction, Int)
```

Given a coordinate pair `(x, y)`, the _execution_ of a command consists in
incrementing the corresponding coordinate.

So for example, executing `(MoveLeft, 10)` on the pair `(5, 5)` should result in
`(-5, 5)`. (We use the mathematical indexing: "right" means increasing the x
coordinate and "up" means increasing the y coordinate).

### Implementation Task

Write a function which, given an initial position `(x, y)`, computes the final
position after the execution of a list of commands.

```haskell
executeCommands :: [Command] -> (Int , Int) -> (Int , Int)
executeCommands = undefined
```

### Examples

```hs
ghci> executeCommands [(MoveRight,10),(MoveLeft,5),(MoveUp,20)] (0,0)
(5,20)
```

## Question 5 [10 marks]

### Background Material

ATMs need to use an algorithm to compute, given an amount, the number of
currency notes needed to dispense the money requested by the customer.
In this question, you will implement such an algorithm in Haskell.

### Implementation Task

Implement a Haskell function `atmChange`:

```haskell
atmChange :: Int -> [Int] -> [(Int, Int)]
atmChange = undefined
```

which takes an amount and a list of denominations in ascending order (such as
`[10, 20, 50, 100]`) and returns a list of pairs of denominations and number of
notes to be dispensed (with the fewest number of notes).

### Examples

```hs
ghci> atmChange 2180 [10, 50, 100, 500]
[(500,4),(100,1),(50,1),(10,3)]
```

In the above example, the user has asked for £2180 with allowed denominations of
10, 50, 100, and 500. The algorithm computes that the ATM will need to dispense
4 bills of 500, 1 bill of 100, 1 bill of 50 and 3 bills of 10.

Note that 2180 = 4 x 500 + 1 x 100 + 1 x 50 + 3 x 10 

Some more examples follow.

```hs
ghci> atmChange 3270 [10, 50, 100, 500]
[(500,6),(100,2),(50,1),(10,2)]

ghci> atmChange 3270 [10, 50, 100, 200, 500]
[(500,6),(200,1),(100,0),(50,1),(10,2)]

ghci> atmChange 3275 [1, 20, 50, 100, 500, 1000]
[(1000,3),(500,0),(100,2),(50,1),(20,1),(1,5)]
```
