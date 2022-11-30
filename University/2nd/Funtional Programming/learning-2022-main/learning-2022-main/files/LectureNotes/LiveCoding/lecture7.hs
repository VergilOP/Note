--
--  lecture7.hs
--

--
--  Attendance: 87801403
--

data BT a = Empty
          | Fork a (BT a) (BT a)
          deriving Show

treeInOrder :: BT a -> [a]
treeInOrder Empty = []
treeInOrder (Fork x l r) = treeInOrder l ++ [x] ++ treeInOrder r

treePreOrder :: BT a -> [a]
treePreOrder Empty = []
treePreOrder (Fork x l r) = [x] ++ treePreOrder l ++ treePreOrder r

treePostOrder :: BT a -> [a]
treePostOrder Empty = []
treePostOrder (Fork x l r) = treePostOrder l ++ treePostOrder r ++ [x]

--
--  Binary Search Tree
--

-- all :: (a -> Bool) -> [a] -> Bool

allBT :: (a -> Bool) -> BT a -> Bool
allBT p Empty = True
allBT p (Fork x l r) = (p x) && allBT p l && allBT p r
  
isBST :: Ord a => BT a -> Bool
isBST Empty = True
isBST (Fork x l r) = isBST l &&
                     isBST r &&
                     allBT (x >) l &&
                     allBT (x <) r 
                    

tr = Fork 7
     (Fork 3
      (Fork 1 Empty Empty)
      (Fork 4 Empty Empty))
     (Fork 9 Empty Empty)

insert :: Ord a => a -> BT a -> BT a
insert y Empty = Fork y Empty Empty
insert y (Fork x l r) | y < x = Fork x (insert y l) r
                      | x < y = Fork x l (insert y r)
                      | otherwise = Fork x l r

occurs :: Ord a => a -> BT a -> Bool
occurs y Empty = False
occurs y (Fork x l r) | y < x = occurs y l
                      | x < y = occurs y r
                      | otherwise = True

--
--  More Trees
--

data TTT a b = Leaf b
             | Branch a (TTT a b) (TTT a b)

ttt = Branch 6
      (Leaf 'a')
      (Leaf 'b')


data Rose a = Br a [Rose a]

rose = Br 4 []

(@) :: Rose a -> [Int] -> a
(@) (Br x _) [] = x
(@) (Br x bs) (d:ds) = (bs !! d) @ ds 


data Expr = ConstInt Int
          | ConstBool Bool
          | Add Expr Expr
          | Mul Expr Expr
          | If Expr Expr Expr
          | And Expr Expr
          | Or Expr Expr

eval :: Expr -> Either Bool Int
eval (ConstInt n) = Right n
eval (ConstBool b) = Left b
eval (Add e f) = case (eval e , eval f) of
                   (Right n , Right m) -> Right (n + m)
                   _ -> undefined
eval (Mul e f) = case (eval e , eval f) of
                   (Right n , Right m) -> Right (n * m)
                   _ -> undefined
eval (And e f) = case (eval e , eval f) of
                   (Left b , Left c) -> Left (b && c)
                   _ -> undefined
eval (Or e f) = case (eval e , eval f) of
                   (Left b , Left c) -> Left (b || c)
                   _ -> undefined
eval (If c t e) = case eval c of
                    Left True -> eval t
                    Left False -> eval e

--
-- Json Data
--

data Json = JNull
          | JStr String
          | JNum Float
          | JBool Bool
          | JArr [Json]
          | JObj [(String, Json)]

ex = JObj
     [ ("firstName" , JStr "John")
     , ("lastName" , JStr "Smith")
     , ("age" , JNum 27)
     , ("address" , JObj
                    [ ("street" , JStr "21 2nd St.")
                    ])
     ]
      




                      

