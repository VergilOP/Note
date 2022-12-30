# Problem Sheet for Weeks 7 and 8

## Using Maybe Types

1. Rewrite the `head` and `tail` functions from the prelude so that
    they use the `Maybe` type constructor to indicate when provided
    the argument was empty.

	```haskell
    headMaybe :: [a] -> Maybe a
    headMaybe [] = Nothing
    headMaybe (x:_) = Just x

    tailMaybe :: [a] -> Maybe [a]
    tailMaybe [] = Nothing
    tailMaybe (_:xs) = Just xs
    ```

1. Similarly, rewrite `take :: Int -> [a] -> [a]` to use `Maybe` to indicate
    when the index is longer than the list.

	```haskell
    takeMaybe :: Int -> [a] -> Maybe [a]
    takeMaybe n [] | n == 0 = Just []
                              | otherwise = Nothing
	takeMaybe 0 x = Just []
    takeMaybe n (x:xs) =
        case takeMaybe (n-1) xs of
              Nothing -> Nothing
              Just r -> Just (x:r)
    ```

1.  A common use of the `Either` type constructor is to return information
    about a possible error condition.  Rewrite the function `zip` from the
	prelude as
	```hs
	zipEither :: [a] -> [b] -> Either String [(a,b)]
	```
	so that we only get the list of pairs when the two arguments have
	the same length.  If this is not the case, use the `String` to report
	which argument was smaller.

	```haskell
	zipEither :: [a] -> [b] -> Either String [(a,b)]
	zipEither [] [] = Right []
	zipEither [] (_:_) = Left "Left list too small"
	zipEither (_:_) [] = Left "Right list too small"
	zipEither (x:xs) (y:ys) =
	  case zipEither xs ys of
		Left msg -> Left msg
		Right r -> Right ((x,y):r)
	```

## Type Retractions

1. Recall the data type
    ```hs
	data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun
				   deriving (Show, Read, Eq, Ord, Enum)
    ```
	defined in the Lecture Notes.  Define a new data type which
	represents just the **working days** of the week.  Show that
	it is a retract of the above type.

    You are expected to defined the following type and write the
    following two functions:

	```haskell
	data WorkingDay = Mon' | Tue' | Wed' | Thu' | Fri'
	  deriving (Eq, Show)

	toWeekDay :: WorkingDay -> WeekDay
	toWeekDay Mon' = Mon
	toWeekDay Tue' = Tue
	toWeekDay Wed' = Wed
	toWeekDay Thu' = Thu
	toWeekDay Fri' = Fri

	toWorkingDay :: WeekDay -> WorkingDay
	toWorkingDay Mon = Mon'
	toWorkingDay Tue = Tue'
	toWorkingDay Wed = Wed'
	toWorkingDay Thu = Thu'
	toWorkingDay Fri = Fri'
	toWorkingDay Sat = Fri'
	toWorkingDay Sun = Mon'
	```

1.  Show that the type `Maybe a` is a retract of the type `[a]`.
    You are expected to write the following two functions:

	```haskell
    toList :: Maybe a -> [a]
    toList Nothing = []
    toList (Just x) = [x]

    toMaybe :: [a] -> Maybe a
    toMaybe [] = Nothing
    toMaybe (x:_) = Just x
    ```

## User Defined Data Types

1. Define a type of binary trees
    ```hs
    data BinLN a b = ???
    ```
	which carries an element of type `a` at each leaf, and an element
	of type `b` at each node.

	```haskell
    data BinLN a b = Leaf a | Node (BinLN a b) b (BinLN a b)
    ```

1. Using the datatype from the previous problem, write a function
    ```hs
	leaves :: BinLN a b -> [a]
	```
    which collects the list of elements decorating the leaves of the
	given tree.

	```haskell
    leaves :: BinLN a b -> [a]
    leaves (Leaf x) = [x]
    leaves (Node l _ r) = leaves l ++ leaves r 
    ```

1. Implement a new version of binary trees which carries data **only**
   at the leaves.
    ```hs
    data BinL a = ???
    ```
    ```haskell
    data BinL a = Lf a | Nd (BinL a) (BinL a)
    ```

1. Using the datatype from the previous examples, and supposing the type `a`
    has an instance of `Show`, implement a function which renders the tree
	as a collection of parentheses enclosing the elements at the leaves.
    ```hs
    showBin :: Show a => BinL a -> String
	```
	For example:
    ```hs
	*Main> showBin (Nd (Lf 1) (Nd (Lf 3) (Lf 4)))
    "((1)((3)(4)))"
	```
    ```haskell
    showBin :: Show a => BinL a -> String
    showBin (Lf a) = "(" ++ show a ++ ")"
    showBin (Nd l r) = "(" ++ showBin l ++ showBin r ++ ")"
	```
1. **Harder** Can you write a function which, given such a well parenthesized string
    of numbers, produces the corresponding tree?  You may want to use
	`Maybe` or `Either` to report when this string is ill-formed.  (You
	may wish to look up the `read` function for help converting strings
	to integer types.)

	```haskell
	type MaybeReader a = String -> Maybe (a,String)

	mrInt :: MaybeReader Int
	mrInt s = case reads s of
	  [(i,r)] -> Just (i,r)
	  _ -> Nothing

	mrLeftParen :: MaybeReader ()
	mrLeftParen ('(':r) = Just ((),r)
	mrLeftParen _ = Nothing

	mrRightParen :: MaybeReader ()
	mrRightParen (')':r) = Just ((),r)
	mrRightParen _ = Nothing

	mrSeq :: MaybeReader a -> MaybeReader b -> MaybeReader (a,b)
	mrSeq x y s =
	  case x s of
		Nothing -> Nothing
		Just (a,r) ->
		  case y r of
			Nothing -> Nothing
			Just (b,q) -> Just ((a,b),q)

	mrChoice :: MaybeReader a -> MaybeReader b -> MaybeReader (Either a b)
	mrChoice x y s =
	  case x s of
		Nothing -> case y s of
					 Nothing -> Nothing
					 Just (b,r) -> Just (Right b, r)
		Just (a,r) -> Just (Left a , r)

	mrParens :: MaybeReader a -> MaybeReader a
	mrParens x s =
	  case (mrLeftParen `mrSeq` (x `mrSeq` mrRightParen)) s of
		Nothing -> Nothing
		Just (((),(a,())),r) -> Just (a,r)

	parseLeaf :: MaybeReader (BinL Int)
	parseLeaf s = case mrParens mrInt s of
	  Nothing -> Nothing
	  Just (i,r) -> Just (Empty i,r)

	parseBranch :: MaybeReader (BinL Int)
	parseBranch s =
	  let br = mrParens (parseBin `mrSeq` parseBin) in
		case br s of
		  Nothing -> Nothing
		  Just ((l,r),rem) -> Just (Branch l r , rem)

	parseBin :: MaybeReader (BinL Int)
	parseBin s =
	  case mrChoice parseLeaf parseBranch s of
		Nothing -> Nothing
		Just (Left b,r) -> Just (b,r)
		Just (Right b,r) -> Just (b,r)
	```

## Additional exercises on trees

1. Define the _right grafting_ operation
	```haskell
	(//) :: BT a -> BT a -> BT a 
    (//) Empty s = s
    (//) (Fork x l r) s = Fork x l (r // s)
	```
1. Do the same for _left grafting_
	```haskell
	(\\) :: BT a -> BT a -> BT a 
    (\\) Empty s = s
    (\\) (Fork x l r) s = Fork x (l \\ s) r
	```
1. Given a binary tree, let us label the leaves from left to right starting at 0.  Each node then determines a pair of integers `(i,j)` where `i` is the index of its left-most leaf and `j` is the index of its rightmost leaf.  Write a function:
	```haskell
	leafIndices :: BT a -> BT (Int,Int)
	leafIndices = undefined
	```
	Which replaces each node with the pair `(i,j)` of indices of its left and right-most leaves.
	```haskell
	leafIndicesAcc :: Int -> BT a -> (BT (Int,Int) , Int)
	leafIndicesAcc i Empty = (Empty,i+1)
	leafIndicesAcc i (Fork _ l r) =
	  let (l',i') = leafIndicesAcc i l
		  (r',i'') = leafIndicesAcc i' r
	  in (Fork (i,i''-1) l' r' , i'')

	leafIndices :: BT a -> BT (Int, Int)
	leafIndices t = fst $ leafIndicesAcc 0 t
	```

1.  Recall the data type we developed in [lecture](https://git.cs.bham.ac.uk/fp/learning-2022/-/blob/main/files/LectureNotes/LiveCoding/lecture7.hs) for [JSON](https://en.wikipedia.org/wiki/JSON) data.
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
	```
	so that `allValues json key` recursively finds all values associated with the string `key` occuring in instances of the `JObj` constructor.
	```haskell
	allValues :: Json -> String -> [Json]
    allValues (JArr vs) key = concat $ map (\v -> allValues v key) vs  
    allValues (JObj kvs) key = (concat $ map (\(_,v) -> allValues v key) kvs) ++
                           [ v | (k,v) <- kvs , k == key]
    allValues _ key = []                            
	```
