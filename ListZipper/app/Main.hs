module Main where

import ListZipperImpl

main :: IO ()
main = do
    let xs = [1,2,3,4]
        res1 = goForward (xs, [])
        res2 = goForward res1
        res3 = goForward res2
        res4 = goBack res3
    mapM_ print [res1,res2,res3,res4]
