module Main where

import ZipperImpl

main :: IO ()
main = do
    let newForcus = (freeTree, []) -: goLeft -: goRight -: modify (\_ -> 'P')
        newForcus2 = newForcus -: goUp -: modify (\_ -> 'X')
        farLeft = (freeTree, []) -: goLeft -: goLeft -: goLeft -: goLeft
        newForcus3 = farLeft -: attach (Node 'Z' Empty Empty)
        result = topMost newForcus3
    mapM_ print [newForcus, newForcus2, newForcus3, result]
