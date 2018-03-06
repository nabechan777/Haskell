module Main where

import RPNImpl

main :: IO ()
main = do
    expression <- getLine
    if expression == "quit"
        then return ()
        else return (solveRPN expression) >>= print >> main
