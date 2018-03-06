module Main where

import EuclidImpl

main :: IO ()
main = do
    numbers <- putStrLn "Input two number." >> fmap (makePair . map read . words) getLine :: IO (Int, Int)
    let runTypes = [Type1, Type2]
        results = map (\runType -> findGreatestCommonFacter runType (fst numbers) (snd numbers)) runTypes
        zipFsts = zipWith (\runType result -> show runType ++ ": " ++ show (fst result)) runTypes results
        zipSnds = zipWith (\runType result -> show runType ++ ": " ++ show (snd result)) runTypes results
    putStrLn "Result >> " >> mapM_ putStrLn zipFsts 
    putStrLn "Log >>  " >> mapM_ putStrLn zipSnds

makePair :: [Int] -> (Int, Int)
makePair (x:y:_) = (x, y)
