module Main where

import HeathrowImpl

main :: IO ()
main = do
    contents <- getContents
    let (pathString, pathTime) = findShortestPath contents
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "Time taken: " ++ show pathTime
