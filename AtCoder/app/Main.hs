module Main where

import Lib

main :: IO ()
main = do
    r <- getLine
    putStrLn (acCapted3 $ (acCepted1 r >>= acCepted2))
