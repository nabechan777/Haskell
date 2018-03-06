module StackImpl where

import Control.Monad.State

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackActionImpl :: String -> Int -> Stack -> Stack
stackActionImpl "pop" _ stack = snd $ runState pop stack
stackActionImpl "push" x stack = snd $ runState (push x) stack

stackAction :: Stack -> IO ()
stackAction stack = do
    putStrLn "Please enter the command."
    enteredString <- fmap words getLine
    let cmd = enteredString !! 0
        value = read (enteredString !! 1)
        newStack = stackActionImpl cmd value stack
    putStrLn $ show newStack
    stackAction newStack
