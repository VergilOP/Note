# ClassTest2

## Preparation
* Do __not__ modify the files `Types.hs` and `ClassTest2-Template.hs`.
* Copy the file `ClassTest2-Template.hs` to a new file called `ClassTest2.hs` and write your solutions in `ClassTest2.hs`.

  __Don't change the header of this file, including the module declaration, and, moreover, don't change the
type signature of any of the given functions for you to complete.__

* Solve the exercises below in the file `ClassTest2.hs`.

## Plagiarism

Plagiarism will not be tolerated. Copying and contract cheating has led to full loss of marks, and even module or degree failure, in the past.

You will need to sign a declaration on Canvas, before submission, that you understand the [rules](/plagiarism) and are abiding by them, in order for your submission to qualify.

## Background material

- Each question has some **Background Material**, an **Implementation Task** and some **Examples**.
- Read this material first, then implement the requested function.
- The corresponding type appears in the file `ClassTest2-Template.hs` (to be copied by you).
- Replace the default function implementation of `undefined` with your own function.

## Exercise 1 - Decimal to Factorial and Back

### Implementation Task

Coding decimal numbers with factorials is a way of writing out numbers in a base system that depends on factorials, rather than powers of numbers.

In this system, the last digit is always `0` and is in base 0!. The digit before that is either `0 or 1` and is in base 1!. The digit before that is either `0, 1, or 2` and is in base 2!, etc. More generally, the nth-to-last digit is always `0, 1, 2, ..., n` and is in base n!.

Read more about it at: http://en.wikipedia.org/wiki/Factorial_number_system

### Example
The decimal number `463` is encoded as `"341010"`, because:

`463 = 3×5! + 4×4! + 1×3! + 0×2! + 1×1! + 0×0!`

If we are limited to digits 0..9, the biggest number we can encode is 10!-1 (= 3628799). So we extend 0..9 with letters A..Z. With these 36 digits we can now encode numbers up to 36!-1 (= 3.72 × 1041)

### Task
We will need two functions. The first one will receive a decimal number and return a string with the factorial representation.

The second one will receive a string with a factorial representation and produce the decimal representation.

Given numbers will always be positive.

I defined `factorial` for you 

```hs
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)
```

Hint: You can use `toIntegral` to translate `Int` to `Integer`

### Examples

```hs
> dec2FactString 463
"341010"
> dec2FactString 8999
"15243210"
> factString2Dec "341010"
463
> factString2Dec "15243210"
8999
```

## Exercise 2 - Sort binary tree by levels

You are given a binary tree:

```hs
data TreeNode a = TreeNode {
  left  :: Maybe (TreeNode a),
  right :: Maybe (TreeNode a),
  value :: a
  } deriving Show
```

Your task is to return the list with elements from tree sorted by levels, which means the root element goes first, then root children (from left to right) are second and third, and so on.

Return empty list if root is `Nothing`.

Example 1 - following tree:
```
                 2
            8        9
          1  3     4   5
```

Should return following list:

`[2,8,9,1,3,4,5]`

Example 2 - following tree:

```
                 1
            8        4
              3        5
                         7
```
Should return following list:

`[1,8,4,3,5,7]`

## Exercise 3 - The Binary Tree, or There and Back Again

In this Kata you will rotate a binary tree. You need to implement two methods to rotate a binary tree: one to rotate it to the `left` and one to rotate it to the `right`.

If rotation is impossible, return the tree unchanged.

### Tree structure

```hs
data Tree a = Empty
            | Node { left', right' :: Tree a , value' :: a }
            deriving (Show,Eq,Foldable)
```

### What is a binary tree?
A binary tree is a tree graph, in which each element can't have more than `2` children.
Values can not be duplicated, so (sub)trees can be associated with, and denoted by, their value.

### What does rotate mean?

What does it mean to rotate a binary tree in this case? The rotation changes the root of the tree to be the left or right child of the current root. For example:

```

      9
     / \
    7   11
   / \
  5   8
```
In this case the root is `9`, its left child is `7`, and its right child is `11`.

If we rotate it to the right, we'll get this tree:

```
    7
   / \
  5   9
     / \
    8   11
```

We move the left child of the old root (`7`) to be the new root, and move its right child (`8`) to be the new left child of the old root.

If we rotate it to the left, we'll get another tree:

```
       11
       /
      9
     /
    7
   / \
  5   8
```

We move the right child of the old root (`11`) to be the new root, and move its left child (`null` in this case) to be the new right child of the old root.

## Exercise 4 - Index for each leaf

`data BT a = Empty' | Fork a (BT a) (BT a)`

Given a binary tree, let us label the leaves from left to right starting at 0.  Each node then determines a pair of integers `(i,j)` where `i` is the index of its left-most leaf and `j` is the index of its rightmost leaf.  Write a function:
```haskell
leafIndices :: BT a -> BT (Int,Int)
leafIndices = undefined
```
Which replaces each node with the pair `(i,j)` of indices of its leftand right-most leaves.
For example, the tree:
```
	   a
	  /  \
	 b    c
	/ \  / \ 
	        d
	       / \
```
would be mapped to the tree
```
	  (0,4)
	  /    \
	(0,1)  (2,4)
	 / \   /   \ 
	          (3,4)
	          /   \
```

### Exercise 5 - Trees to Parentheses, and Back

Binary trees can be encoded as strings of balanced parentheses (in fact, the two things are isomorphic). Your task is to figure out such an encoding, and write the two functions which convert back and forth between the binary trees and strings of parentheses.

Here's the definition of binary trees:

```hs
data Tree' = Leaf | Tree' :*: Tree' deriving (Eq, Show)
```

And here are the functions you need to define:

```hs
treeToParens :: Tree -> String
parensToTree :: String -> Tree
```

The first function needs to return only strings of valid balanced parentheses (like "()(())").
The second needs to accept any string of balanced parentheses.

Also, the functions need to be inverses of each other.
In other words, they need to satisfy the following equations:

```hs
forall s. treeToParens (parensToTree s) = s
forall t. parensToTree (treeToParens t) = t
```

Note:

There is more than one possible answer to this puzzle! There are number of different ways to "encode" a tree as a string of parentheses. Any solution that follows the laws above will be accepted.


### Exercise 6 - Text messaging
(From "Haskell Programming")

Remember old-fashioned phone inputs for writing text, where
you had to press a button multiple times to get different letters to
come up? You may still have to do this when you try to search for a
movie to watch using your television remote control. You’re going to
write code to translate sequences of button presses into strings and
vice versa.
Here is the layout of the phone:
```
-----------------------------------------
| 1      | 2 ABC | 3 DEF  |
-----------------------------------------
| 4 GHI  | 5 JKL | 6 MNO  |
-----------------------------------------
| 7 PQRS | 8 TUV | 9 WXYZ |
-----------------------------------------
| *      | 0     | # .,   |
-----------------------------------------
```
The star (`*`) capitalizes the next letter (if you press it twice, then it reverts to lower case).  If there are multiple occurrences of `*` in a row, then it is the last one which will determine the capitalization.  `0` is your space bar. To represent the digit itself, you press that digit once more than the letters it represents. If you press a button one more than is required to type the digit, it wraps around to the first letter. For example:
```
2 -> 'a'
22 -> 'b'
222 -> 'c'
2222 -> '2'
22222 -> 'a'
0 -> ' '
00 -> '0'
000 -> ' '
1 -> '1'
11 -> '1'
111 -> '1'
```
You will not need to type '#', so
```
# -> '.'
## -> ','
### -> '.'
```

 Consider the following datatypes:
```haskell
-- Valid buttons are ['0'..'9']++['*','#']
type Button = Char
-- Valid presses are [1..]
type Presses = Int
-- Valid text consists of
-- ['A'..'Z']++['a'...'z']++['0'..'9']++['.',',',' ']
type Text = String
```
#### Exercise 6a.
Write a function
```haskell
phoneToString :: [(Button, Presses)] -> Text
phoneToString = undefined
```
that takes a list of buttons and the number of times to press them and gives back the corresponding text, e.g.
```hs
phoneToString [('*',1),('6',5),('5',4)] = "M5"
```

#### Exercise 6b.
Write a function
```haskell
stringToPhone :: Text -> [(Button, Presses)]
stringToPhone = undefined
```
taking a string to a list of buttons and the number of times that they need to be pressed, e.g.
```hs
stringToPhone "Hi, students." = [('*',1),('4',2),('4',3),('#',2),('0',1),('7',4),('8',1),('8',2),('3',1),('3',2),('6',2),('8',1),('7',4),('#',1)]
```

#### Exercise 6c.
Write a function
```haskell
fingerTaps :: Text -> Presses
fingerTaps = undefined
```
that computes the minimal number of button presses needed to input the given string, e.g.
```hs
fingerTaps "Hi, students." = 27
```

## Exercise 7 - Computing Factorial with the State Monad

Consider the following Java method:
```java
int fac (int n) {
  int y = 1;
  while (n > 1} {
   System.out.println("n = " + n);
   y = y * n;
   n--;
   }
  }
  return y;
```
Write this as a Haskell function using the state monad:
```
facHelper :: Integer -> State Integer ()
facHelper = undefined

factorial :: Integer -> Integer
factorial n = snd (runstate (facHelper n) 1)
```
Hint: See how we define `fib'` using the state monad in the [monads handout](https://git.cs.bham.ac.uk/fp/learning-2022/-/blob/main/files/LectureNotes/Sections/monads.md#fibstate).

## Exercise 8 - Applying Functions to Trees

### Background Material

There are many possible variants of the type of binary trees
introduced in the Lecture Notes.  For example, consider the following
definition:

```haskell
data Tree'' a b = Leaf' b | Fork' (Tree'' a b) a (Tree'' a b)
  deriving (Eq, Show)
```
Notice how now, the tree stores two different types
of data: an element of type `a` at each fork and an element of type
`b` at each leaf.

### Implementation Task

Your task is to write a higher-order function `applyfuns` that takes
two functions `f :: a -> b` and `g :: b -> d`, as well as an element
of type `Tree a b` as input and applies the first function to the
values found at the forks, and the second function to the values found
at the leaves.  That is, implement the function:

```haskell
applyfuns :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d
applyfuns = undefined
```
### Examples

Lets consider the following two functions:

```haskell
str2int :: String -> Int
str2int xs = length xs

int2bool :: Int -> Bool
int2bool n = n /= 0
```

and the following binary tree:
```
       "John"
       /    \
      /      \
  "Oliver" "Benjamin"
   /   \      /   \
  /     \    /     \
 2       4  0       6
```

Then the expression `applyfuns str2int int2bool` should return the tree
```
          4
       /    \
      /      \
     6          8
   /   \      /   \
  /     \    /     \
True   True False  True
```

```hs
*Main> applyfuns str2int int2bool (Fork (Fork (Leaf 2) "Oliver" (Leaf 4)) "John" (Fork (Leaf 0) "Benjamin" (Leaf 6)))
Fork (Fork (Leaf True) 6 (Leaf True)) 4 (Fork (Leaf False) 8 (Leaf True))

```

As a second example, the tree
```
                   "New York"
                    /      \
                   /        \
              "Paris"      "Dubai"
              /    \	   /    \
             /      \     /      \
        "London"    14   5    "Shanghai"
           /   \                /     \
          /     \              /       \
         0      10            0        21
```
is transformed into the tree
```
                       8
                    /      \
                   /        \
                 5           5
              /    \	   /    \
             /      \     /      \
             6     True True       8
           /   \                /     \
          /     \              /       \
        False  True          False     True
```

```hs
*Main> applyfuns str2int int2bool (Fork (Fork (Fork (Leaf 0) "London" (Leaf 10)) "Paris" (Leaf 14)) "New York" (Fork (Leaf 5) "Dubai" (Fork (Leaf 0) "Shanghai" (Leaf 21))))
Fork (Fork (Fork (Leaf False) 6 (Leaf True)) 5 (Leaf True)) 8 (Fork (Leaf True) 5 (Fork (Leaf False) 8 (Leaf True)))

```

## Exercise 9 - Updating Nodes Along a Route

### Background Material

In this exercise, we return to a version of binary trees which stores data only at the nodes.  We will use the following definition:
```haskell
data BinTree a = Empty'' | Node' (BinTree a) a (BinTree a)
  deriving (Eq, Show)
```

Next, we define a type `Route` which will describe the "route" one must take to arrive at a particular node in one of our trees.

```haskell
data Direction = GoLeft | GoRight
  deriving (Eq, Show, Bounded, Enum)

type Route = [Direction]
```

So a route is a list of directions, which tell you whether to go left or right,
starting from the root of the binary tree. For example,

1. The route to the root of any binary tree is `[] :: Route`.
1. In the tree `Node (Node Empty 'b' Empty) 'a' (Node (Node Empty 'd'
   Empty) 'c' (Node Empty 'e' Empty))`, pictured below, the route `[GoLeft]`
   takes you to the node with value 'b', while the routes to the nodes
   with values 'd' and 'e' are `[GoRight,GoLeft]` and
   `[GoRight,GoRight]` respectively.
   ```
                  'a'
                  / \
                 /   \
               'b'   'c'
                     / \
                    /   \
                  'd'   'e'
   ```

### Implementation Task

Your task is to implement the function
```haskell
updateNodes :: Route -> (a -> a) -> BinTree a -> BinTree a
updateNodes = undefined
```
such that `updateNodes r f t` applies `f` to the values of all nodes along the route `r` in the tree `t`.

**NB**:
1. If you run out of directions before hitting a leaf, e.g. if running
`updateNodes` on the route `[GoRight]` in the tree above, then you
stop and do **not** modify the remainder of the tree (the values `'d'`
and `'e'` in the example).
1. If the route is too long, e.g. if running `updateNodes` on the
route `[GoLeft,GoLeft]` in the tree above, then you discard the
remainder of the route (so in the example, you would only update the
values `'a'` and `'b'` and then stop).

The examples given below should also help to clarify these points.

### Examples

For the following binary tree:
```
       1
      / \
     2   \
    / \	  99
   /   \   \
  3     4   \
            100
```

```hs
*Main> let t = Node (Node (Node Empty 3 Empty) 2 (Node Empty 4 Empty)) 1 (Node Empty 99 (Node Empty 100 Empty))
*Main> updateNodes [] (*8) t
Node (Node (Node Empty 3 Empty) 2 (Node Empty 4 Empty)) 8 (Node Empty 99 (Node Empty 100 Empty))
*Main> updateNodes [GoRight] (*8) t
Node (Node (Node Empty 3 Empty) 2 (Node Empty 4 Empty)) 8 (Node Empty 792 (Node Empty 100 Empty))
*Main> updateNodes [GoRight,GoLeft] (*8) t
Node (Node (Node Empty 3 Empty) 2 (Node Empty 4 Empty)) 8 (Node Empty 792 (Node Empty 100 Empty))
*Main> updateNodes [GoRight,GoRight] (*8) t
Node (Node (Node Empty 3 Empty) 2 (Node Empty 4 Empty)) 8 (Node Empty 792 (Node Empty 800 Empty))
*Main> updateNodes [GoRight,GoRight,GoLeft] (*8) t
Node (Node (Node Empty 3 Empty) 2 (Node Empty 4 Empty)) 8 (Node Empty 792 (Node Empty 800 Empty))
*Main> updateNodes [GoLeft,GoLeft,GoLeft] (*15) t
Node (Node (Node Empty 45 Empty) 30 (Node Empty 4 Empty)) 15 (Node Empty 99 (Node Empty 100 Empty))
```

**NB**: Remember that we are not storing the results of running `updateNodes`
above, so all examples above are run on the same tree
`t = Node (Node (Node Empty 3 Empty) 2 (Node Empty 4 Empty)) 1 (Node Empty 99 (Node Empty 100 Empty))`.

## Exercise 10 - Monadic Calculator

### Background Material

The modules `Monad.Control.Except` and `Monad.Control.State` contain
definitions of various extensions of the `Monad` class.  We can use these
to write functions which work for **any** monad `m` satisfying the correct
interface.

For example, the `MonadError` class, defined [here](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Error-Class.html#t:MonadError),
adds a method
```haskell
throwError :: e -> m a
```
allowing you to return an error of type `e`.

Similarly, the `MonadState` class, defined [here](https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-State-Class.html#t:MonadState) adds
methods
```haskell
get :: m s
put :: s -> m ()
modify :: MonadState s m => (s -> s) -> m ()
```
which allow you to manipulate the state carried by the monad `m`.

### Implementation Tasks

1. Consider the following type of calculator expressions:

	```haskell
	data CalcExpr = Val Int
                  | Add CalcExpr CalcExpr
                  | Mult CalcExpr CalcExpr
                  | Div CalcExpr CalcExpr
                  | Sub CalcExpr CalcExpr
	```
	Write an evaluator which runs in any monad supporting exceptions and which throws an error when it
	encounters a division by zero.
    ```haskell
	eval :: MonadError String m => CalcExpr -> m Int
	```
	Notice how we have specialized the error type `e` from `MonadError` to `String` here.  This means that
	when you encounter a divide by zero, you should return an error message as a string.

1. Now let's imagine a calculator with an integer state which allows the user to update this state using
   commands. Here is a data type describing a list of commands:
    ```haskell
    data CalcCmd = EnterC
                 | StoreC Int CalcCmd
                 | AddC Int CalcCmd
                 | MultC Int CalcCmd
                 | DivC Int CalcCmd
                 | SubC Int CalcCmd
	```
	Write a function
	```hs
	run :: (MonadState Int m, MonadError String m) => CalcCmd -> m ()
	```
	which runs the given sequence of commands in any monad supporting state and exceptions.
    Each of the `AddC`, `MultC`, `DivC` and `SubC` commands should apply
    the corresponding operation on the provided argument and whatever
    the current state is.  The `StoreC` command manually updates the
    state. Finally, `EnterC` terminates the calculation, returning the
    unit type.

### Examples

1. Here are two calculator expressions:

	```haskell
	expr1 = Mult (Add (Val 4) (Val 7)) (Div (Val 10) (Val 2))
	expr2 = Sub (Val 10) (Div (Val 14) (Val 0))
	```

	The `Either` type implements the required monadic interface.  Hence we can evaluate using
	this type as follows:
	```
	ghci> eval expr1 :: Either String Int
	Right 55
	ghci> eval expr2 :: Either String Int
	Left "Divide by zero!"
	ghci>
	```

2. Now here are two command sequences:

    ```haskell
	cmd1 = StoreC 7 (AddC 14 (DivC 3 EnterC))
	cmd2 = StoreC 10 (MultC 2 (DivC 0 EnterC))
	```
	To run these, we will need to choose an implementation of the state monad to use.  We can do this
	by introducting the following type synonym:
	```haskell
	type CS a = StateT Int (Either String) a
	```
	Now we can do:
	```
	ghci> runStateT (run cmd1 :: CS ()) (0 :: Int)
	Right ((),7)
	ghci> runStateT (run cmd2 :: CS ()) (0 :: Int)
	Left "Divide by zero!"
	```
	The value `7` in `Right ((),7)` is showing us the resulting state of the calculator after the
	sequence of commands.  This makes sense: we first store `7`, then add `14` to the stored value
	and then divide by `3`, leaving a result of `7`.

