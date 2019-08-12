#!/usr/bin/env stack
-- stack script --resolver lts-9.21
{-# LANGUAGE OverloadedStrings #-}
module TutorialExercise
    ( basicUsage
    ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple

main :: IO ()
main = basicUsage

basicUsage :: IO ()
basicUsage = do
    response <- httpLBS "http://httpbin.org/get"

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response
