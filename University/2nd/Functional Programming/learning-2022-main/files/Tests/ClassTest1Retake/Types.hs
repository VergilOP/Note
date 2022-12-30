{-# LANGUAGE Safe #-}

module Types where

data EditCommand = MoveLeft | MoveRight | Insert Char | BackSpace

type Text = (String, String)

cmds = [ MoveRight
       , MoveRight
       , BackSpace
       , MoveRight
       , Insert 'l'
       , MoveLeft
       , MoveLeft
       , MoveLeft
       ]

cmds2 = [ MoveRight
        , MoveRight
        , Insert 'x'
        , Insert 'y'
        , Insert 'z'
        , MoveLeft
        , BackSpace
        ] 

f (a : b : c : _) = a || b || c
g (a:_) = a && not a
