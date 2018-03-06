module Main where

import RopeWalkingImpl

main :: IO ()
main = do
    let initializeBirds = (0, 0)
        types = [Type1, Type2]
        result = map (flip simulationRopeWalking initializeBirds) types
        zipResult = zipWith (\aType aResult -> show aType ++ ": " ++ show aResult) types result
    mapM_ putStrLn zipResult
