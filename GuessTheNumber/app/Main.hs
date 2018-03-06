module Main where

import System.Random
import GuessTheNumberImpl

main :: IO ()
main = do
    gen <- getStdGen
    askForNumber gen
