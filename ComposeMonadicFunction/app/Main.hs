module Main where

import ComposeMonadicFunctionImpl

main :: IO ()
main = do
    value <- fmap (read . (!! 0) . words) getLine :: IO Int 
    print $ composeFunction value
    print $ composeMonadicFunction value
