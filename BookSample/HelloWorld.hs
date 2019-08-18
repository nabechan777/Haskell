#! /usr/bin/env stack
-- stack script --resolver lts-9.21 --package base,text,bytestring HelloWorld.hs

{-# LANGUAGE OverloadedStrings #-}

import Data.Semigroup
import Data.Char
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Char8 as BC


helloPerson1 :: String -> String
helloPerson1 name = "Hello" ++ " " ++ name ++ "!"


helloPerson2 :: T.Text -> T.Text
helloPerson2 name = "Hello" <> " " <> name <> "!"


helloPerson3 :: BC.ByteString -> BC.ByteString
helloPerson3 name = "Hello" <> " " <> name <> "!"


actionOnString :: IO ()
actionOnString = do
    putStrLn "Hello! What's your name?"
    name <- getLine
    let statement = helloPerson1 name
    putStrLn statement


actionOnText :: IO ()
actionOnText = do
    TIO.putStrLn "Hello! What's your name?"
    name <- TIO.getLine
    let statement = helloPerson2 name
    TIO.putStrLn statement


actionOnByteString :: IO ()
actionOnByteString = do
    BC.putStrLn "Hello! What's your name?"
    name <- BC.getLine
    let statement = helloPerson3 name
    BC.putStrLn statement


selectPattern :: Maybe Int -> IO ()
selectPattern Nothing = doNotSetArg
    where
        doNotSetArg = putStrLn "set argument! (String: 1, Text: 2, ByteString: 3)"
selectPattern (Just pattern) = if pattern > 0 && pattern <= 3 then selectPattern' pattern else outOfBoundsArg
    where
        outOfBoundsArg = putStrLn "set out of Bounds argument! (String: 1, Text: 2, ByteString: 3)"
        selectPattern' 1 = putStrLn "<< select String function >>" >> actionOnString
        selectPattern' 2 = putStrLn "<< select Text function >>" >> actionOnText
        selectPattern' 3 = putStrLn "<< select ByteString function >>" >> actionOnByteString


checkArgs :: [String] -> Maybe Int
checkArgs xs = if existSecondArg && canConversion then (Just $ read secondArg) else Nothing
    where
        existSecondArg = length xs >= 2
        secondArg = xs !! 1
        canConversion = foldl (\acc x -> acc && isDigit x) True secondArg


main :: IO ()
main = do
    args <- getArgs
    (selectPattern . checkArgs) args
