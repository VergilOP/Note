--
--  lecture10 - creating an imperative language
--

import System.Environment
import Control.Monad
import Parsing

type Identifier = String

data Program = Identifier := Expr
             | Block [Program]
             | While Expr Program 

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Eq Expr Expr
          | Gt Expr Expr
          | Constant Integer
          | Variable Identifier

type Storage = Identifier -> Integer

emptyStorage :: Storage
emptyStorage id = error $ "Variable not in scope: " ++ id

updateStorage :: Identifier -> Integer -> Storage -> Storage
updateStorage id x s id' | id == id' = x
updateStorage id x s id' | otherwise = s id' 

-- updateStorage :: Identifier -> Integer -> Storage -> Storage
-- updateStorage id x s = \ id' -> if (id == id') then x else s id' 

number :: Bool -> Integer
number True = 1
number False = 0 

boolean :: Integer -> Bool
boolean i | i == 0 = False
boolean i | otherwise = True

eval :: Storage -> Expr -> Integer
eval s (Add e e') = (eval s e) + (eval s e')
eval s (Sub e e') = (eval s e) - (eval s e')
eval s (Eq e e') = number $ (eval s e) == (eval s e')
eval s (Gt e e') = number $ (eval s e) > (eval s e')
eval s (Constant x) = x
eval s (Variable id) = s id
  
run :: Program -> Storage -> Storage
run (id := e) s = updateStorage id (eval s e) s
run (Block []) s = s 
run (Block (p:ps)) s = run (Block ps) (run p s)
run (While e p) s = if boolean $ eval s e
                    then run (While e p) (run p s)
                    else s

-- Still need to parse expressions!
expr :: Parser Expr
expr = undefined

--
--  Note: we did not finish parsing expressions in class.  You may refer to
--  the lecture notes if you wish to see how this is done.
--

-- x := 6;
assignment :: Parser Program
assignment = do id <- identifier
                symbol ":="
                e <- expr
                return $ id := e


-- many :: Parser a -> Parser [a]
-- { lskdjflskfdjls } 
block :: Parser Program
block = do symbol "{"
           ps <- many program
           symbol "}"
           return $ Block ps

while :: Parser Program
while = do symbol "while"
           symbol "("
           e <- expr
           symbol ")" 
           p <- program
           return $ While e p 

program :: Parser Program
program = assignment
          <|> block
          <|> while

main :: IO ()
main = do args <- getArgs
          if (length args == 2)
            then do contents <- readFile (args !! 0)
                    case parse program contents of
                      [(p,[])] -> let initialStorage = updateStorage "x" (read $ args !! 1) emptyStorage
                                      finalStorage = run p initialStorage
                                      y = finalStorage "y"
                                  in do putStrLn $ "Your program set y := " ++ show y
                      _ -> putStrLn "Syntax error"
            else putStrLn "Usage: runxy [prog name] [x init val]"
