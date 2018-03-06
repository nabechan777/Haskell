module GuessTheNumberImpl
    ( askForNumber
    ) where

import Control.Monad(when)
import System.Random


askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randomNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
    putStrLn "Which number in the range from 1 to 10 am I thinking of?"
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randomNumber == number
            then putStrLn "You are correct!"
            else putStrLn $ "Sorry, it was " ++ show randomNumber ++ "."
        askForNumber newGen
