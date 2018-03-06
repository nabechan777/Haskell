module Main where

import Coin
import ProbabilityImpl

main :: IO ()
main = do
    return flipThree >>= print
