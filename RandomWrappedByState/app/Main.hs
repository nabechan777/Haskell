module Main where

import Control.Monad
import RandomWrappedByStateImpl

main :: IO ()
main = do
    forever $ do
        putStrLn "Enter the seed."
        seed <- fmap read getLine :: IO Int
        randomAction seed >>= print
