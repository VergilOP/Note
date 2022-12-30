# Problem Sheet for Weeks 7 and 8

## Using Maybe Types

1. Rewrite the `head` and `tail` functions from the prelude so that
    they use the `Maybe` type constructor to indicate when provided
    the argument was empty.

	```haskell
    headMaybe :: [a] -> Maybe a
    headMaybe = undefined

    tailMaybe :: [a] -> Maybe [a]
    tailMaybe = undefined
    ```

1. Similarly, rewrite `take :: Int -> [a] -> [a]` to use `Maybe` to indicate
    when the index is longer than the list.

	```haskell
    takeMaybe :: Int -> [a] -> Maybe [a]
    takeMaybe = undefined
    ```

1.  A common use of the `Either` type constructor is to return information
    about a possible error condition.  Rewrite the function `zip` from the
	prelude as

	```haskell
	zipEither :: [a] -> [b] -> Either String [(a,b)]
    zipEither = undefined
	```
	so that we only get the list of pairs when the two arguments have
	the same length.  If this is not the case, use the `String` to report
	which argument was smaller.

## Type Retractions

1. Recall the data type
    ```haskell
	data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun
				   deriving (Show, Read, Eq, Ord, Enum)
    ```
	defined in the Lecture Notes.  Define a new data type which
	represents just the **working days** of the week.  Show that
	it is a retract of the above type.

    You are expected to defined the following type and write the
    following two functions:

	```haskell
	data WorkingDay = ???

	toWeekDay :: WorkingDay -> WeekDay
    toWeekDay = undefined

	toWorkingDay :: WeekDay -> WorkingDay
    toWorkingDay = undefined
    ```

1.  Show that the type `Maybe a` is a retract of the type `[a]`.
    You are expected to write the following two functions:

	```hs
    toList :: Maybe a -> [a]
    toList = undefined

    toMaybe :: [a] -> Maybe a
    toMaybe = undefined
    ```

## User Defined Data Types

1. Define a type of binary trees
    ```haskell
    data BinLN a b = ???
    ```
	which carries an element of type `a` at each leaf, and an element
	of type `b` at each node.

1. Using the datatype from the previous problem, write a function
    ```haskell
	leaves :: BinLN a b -> [a]
    leaves = undefined
	```
    which collects the list of elements decorating the leaves of the
	given tree.

1. Implement a new version of binary trees which carries data **only**
   at the leaves.
    ```haskell
    data BinL a = ???
    ```

1. Using the datatype from the previous examples, and supposing the type `a`
    has an instance of `Show`, implement a function which renders the tree
	as a collection of parentheses enclosing the elements at the leaves.
    ```haskell
    showBin :: Show a => BinL a -> String
    showBin = undefined
	```
	For example:
    ```hs
	*Main> showBin (Nd (Lf 1) (Nd (Lf 3) (Lf 4)))
    "((1)((3)(4)))"
	```

1. **Harder** Can you write a function which, given such a well parenthesized string
    of numbers, produces the corresponding tree?  You may want to use
	`Maybe` or `Either` to report when this string is ill-formed.  (You
	may wish to look up the `read` function for help converting strings
	to integer types.)

## Additional exercises on trees

1. Define the _right grafting_ operation
	```haskell
	(//) :: BT a -> BT a -> BT a 
	(//) = undefined
	```
	such that `r // s` inserts `s` as the rightmost subtree of `r`.

	For example if `r` is
	```
		   4
		  / \
		 7
		/\
	```
	and `s` is 
	```
		   1
		  / \
	```
	then `r // s` should be 
	```
		   4
		  / \
		 7   1
		/\  / \ 
	```
2. Do the same for _left grafting_
	```haskell
	(\\) :: BT a -> BT a -> BT a 
	(\\) = undefined
	```
3. Given a binary tree, let us label the leaves from left to right starting at 0.  Each node then determines a pair of integers `(i,j)` where `i` is the index of its left-most leaf and `j` is the index of its rightmost leaf.  Write a function:
	```haskell
	leafIndices :: BT a -> BT (Int,Int)
	leafIndices = undefined
	```
	Which replaces each node with the pair `(i,j)` of indices of its left and right-most leaves.

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
	
4.  Recall the data type we developed in [lecture](https://git.cs.bham.ac.uk/fp/learning-2022/-/blob/main/files/LectureNotes/LiveCoding/lecture7.hs) for [JSON](https://en.wikipedia.org/wiki/JSON) data.
	```haskell
	data Json = JNull
		  | JStr String
		  | JNum Float
		  | JBool Bool
		  | JArr [Json]
		  | JObj [(String, Json)]
	```
    Write a function 
	```haskell
	allValues :: Json -> String -> [Json]
	allValues = undefined
	```
	so that `allValues json key` recursively finds all values associated with the string `key` occuring in instances of the `JObj` constructor.

## Homework

This homework will be marked but not counted towards your final grade. You will write your code on JupyterLab and
submit via Canvas (link below).

- Create new directory in your workspace on JupyterLab and upload/copy the files found under the `Homework7` directory.
- Copy the file `Homework7-Template.hs` to a new file called `Homework7.hs`.
- Solve the exercises below in the file `Homework7.hs`.
- Run the pre-submit script to check for any (compilation) errors **before** submitting by running in the terminal:

```bash
$ ./presubmit.sh Homework7
```
- Submit your file `Homework7.hs` via Canvas at: https://canvas.bham.ac.uk/courses/65655/assignments/389041 .
- You also require the file `Types.hs` to be in the same directory as `Homework7.hs`. The file `Types.hs` should **not** be modified, and should **not** be submitted on Canvas.

### Text messaging
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
#### Exercise 7a.
Write a function
```haskell
phoneToString :: [(Button, Presses)] -> Text
phoneToString = undefined
```
that takes a list of buttons and the number of times to press them and gives back the corresponding text, e.g.
```hs
phoneToString [('*',1),('6',5),('5',4)] = "M5"
```

#### Exercise 7b.
Write a function
```haskell
stringToPhone :: Text -> [(Button, Presses)]
stringToPhone = undefined
```
taking a string to a list of buttons and the number of times that they need to be pressed, e.g.
```hs
stringToPhone "Hi, students." = [('*',1),('4',2),('4',3),('#',2),('0',1),('7',4),('8',1),('8',2),('3',1),('3',2),('6',2),('8',1),('7',4),('#',1)]
```

#### Exercise 7c.
Write a function
```haskell
fingerTaps :: Text -> Presses
fingerTaps = undefined
```
that computes the minimal number of button presses needed to input the given string, e.g.
```hs
fingerTaps "Hi, students." = 27
```
