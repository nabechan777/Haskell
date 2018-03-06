module Main where

import MoveKnightImpl

main :: IO ()
main = do
    start <- putStrLn "input start axis." >> fmap read getLine :: IO (Int, Int)
    end <- putStrLn "input end axis." >> fmap read getLine :: IO (Int, Int)
    putStr "Result: " >> print (canReachIn3 start end)
