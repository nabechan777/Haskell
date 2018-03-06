module Main where

import RopeWalkingByEitherImpl

main :: IO ()
main = do
    a <- return (Right (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2)
    b <- return (Right (0,0) >>= landRight 3 >>= landLeft 4 >>= landLeft 1)
    c <- return (Right (0,0) >>= landRight 4 >>= landLeft 4 >>= landRight 1)
    d <- return (Right (0,0) >>= landLeft 1 >>= banana >>= landRight 4)
    mapM_ print [a,b,c,d]
