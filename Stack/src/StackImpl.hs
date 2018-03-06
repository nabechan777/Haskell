module StackImpl
    ( stackAction
    ) where

import Control.Monad

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

stackAction :: Stack -> IO ()
stackAction stack = do
    putStrLn "Please enter the command."
    input <- fmap words getLine
    when((input !! 0) == "pop") $ do
        let result = pop stack
        print result
        stackAction (snd result)
    when((input !! 0) == "push") $ do
        let result = push (read (input !! 1) :: Int) stack
        print result
        stackAction (snd result)


stackManip :: Stack -> (Int, Stack)
stackManip stack = let
    ((), newStack1) = push 3 stack
    (a, newStack2) = pop newStack1
    in pop newStack2
