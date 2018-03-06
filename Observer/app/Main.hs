module Main where

import Data.Char (isDigit)
import Control.Monad (when)
import Observer
import Subject

main :: IO ()
main = loop
    where
        loop :: IO ()
        loop = do
            s <- getLine
            when (and $ map isDigit s) $ do
                let subject = NumberGenerator (read s) [DigitObserver, GraphObserver]
                notify subject
                loop
            return ()
