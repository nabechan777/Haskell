module Main where

import Control.Monad.Writer
import MonadicFunctionImpl

main :: IO ()
main = do
    let result = runWriter $ filterM keepSmall [9,1,5,2,10,3]
    return (fst result) >>= print
    return (snd result) >>= mapM_ putStrLn
    return (powerset [1,2,3]) >>= print
    return (foldM binSmalls 0 [2,8,3,1]) >>= print
    return (foldM binSmalls 0 [2,11,3,1]) >>= print
