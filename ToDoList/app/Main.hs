module Main where

import System.Environment
import ToDoListImpl

main :: IO ()
main = do
    (command:argList) <- getArgs
    dispatch command argList
