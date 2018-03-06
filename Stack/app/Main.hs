module Main where

import StackImpl

main :: IO ()
main = do
    putStrLn "Simulate the stack implemented without using the State monad."
    stackAction []
