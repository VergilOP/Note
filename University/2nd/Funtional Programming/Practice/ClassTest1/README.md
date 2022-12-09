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

## Categorize New Member

### DESCRIPTION:
The Western Suburbs Croquet Club has two categories of membership, Senior and Open. They would like your help with an application form that will tell prospective members which category they will be placed.

To be a senior, a member must be at least 55 years old and have a handicap greater than 7. In this croquet club, handicaps range from -2 to +26; the better the player the lower the handicap.

### Input
Input will consist of a list of pairs. Each pair contains information for a single potential member. Information consists of an integer for the person's age and an integer for the person's handicap.

### Output
Output will consist of a list of string values (in Haskell and C: `Open` or `Senior`) stating whether the respective member is to be placed in the senior or open category.

### Example
```hs
input =  [[18, 20], [45, 2], [61, 12], [37, 6], [21, 21], [78, 9]]
output = ["Open", "Open", "Senior", "Open", "Open", "Senior"]

input = []
output = []
```

## Question 2 [10 marks]

## Rot13

### DESCRIPTION:

ROT13 is a simple letter substitution cipher that replaces a letter with the letter 13 letters after it in the alphabet. ROT13 is an example of the Caesar cipher.

Create a function that takes a string and returns the string ciphered with Rot13. If there are numbers or special characters included in the string, they should be returned as they are. Only letters from the latin/english alphabet should be shifted, like in the original Rot13 "implementation".

### Example

```hs
> rot13 "test"
"grfg"

> rot13 "Test"
"Grfg"
```

## Question 3 [10 marks]

## Playing with digits

### DESCRIPTION:

Some numbers have funny properties. For example:

> 89 --> 8¹ + 9² = 89 * 1

> 695 --> 6² + 9³ + 5⁴= 1390 = 695 * 2

> 46288 --> 4³ + 6⁴+ 2⁵ + 8⁶ + 8⁷ = 2360688 = 46288 * 51

Given a positive integer n written as abcd... (a, b, c, d... being digits) and a positive integer p

- we want to find a positive integer k, if it exists, such that the sum of the digits of n taken to the successive powers of p is equal to k * n.

In other words:

> Is there an integer k such as : (a ^ p + b ^ (p+1) + c ^(p+2) + d ^ (p+3) + ...) = n * k

If it is the case we will return k, if not return -1.

**Note**: n and p will always be given as strictly positive integers.

```hs
digpow 89 1 should return 1 since 8¹ + 9² = 89 = 89 * 1
digpow 92 1 should return -1 since there is no k such as 9¹ + 2² equals 92 * k
digpow 695 2 should return 2 since 6² + 9³ + 5⁴= 1390 = 695 * 2
digpow 46288 3 should return 51 since 4³ + 6⁴+ 2⁵ + 8⁶ + 8⁷ = 2360688 = 46288 * 51
```

## Question 4 [10 marks]

## Take a Number And Sum Its Digits Raised To The Consecutive Powers And ....¡Eureka!!

### DESCRIPTION:

The number `89` is the first integer with more than one digit that fulfills the property partially introduced in the title of this kata. What's the use of saying "Eureka"? Because this sum gives the same number.

In effect: `89 = 8^1 + 9^2`

The next number in having this property is `135`.

See this property again: `135 = 1^1 + 3^2 + 5^3`

We need a function to collect these numbers, that may receive two integers `a`, `b` that defines the range `[a, b]` (inclusive) and outputs a list of the sorted numbers in the range that fulfills the property described above.

### Example

Let's see some cases (input -> output):

```
1, 10 -> [1, 2, 3, 4, 5, 6, 7, 8, 9]

1, 100 -> [1, 2, 3, 4, 5, 6, 7, 8, 9, 89]
```

If there are no numbers of this kind in the range [a, b] the function should output an empty list.

```
90, 100 --> []
```

Enjoy it!!

## Question 5 [10 marks]

## Alphabet war

### Introduction

There is a war and nobody knows - the alphabet war!
There are two groups of hostile letters. The tension between left side letters and right side letters was too high and the war began.

### Task

Write a function that accepts fight string consists of only small letters and return who wins the fight. When the left side wins return Left side wins!, when the right side wins return Right side wins!, in other case return Let's fight again!.

The left side letters and their power:
```
 w - 4
 p - 3
 b - 2
 s - 1
```

The right side letters and their power:
```
 m - 4
 q - 3
 d - 2
 z - 1
```

The other letters don't have power and are only victims.

### Example
```
AlphabetWar("z");        //=> Right side wins!
AlphabetWar("zdqmwpbs"); //=> Let's fight again!
AlphabetWar("zzzzs");    //=> Right side wins!
AlphabetWar("wwwwwwz");  //=> Left side wins!
```