module Main where

import qualified HelloWorld
import qualified Examples

main :: IO ()
main = do
    HelloWorld.run
    Examples.run 
