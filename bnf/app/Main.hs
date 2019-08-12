{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Parser
import Data.Attoparsec.Text

main :: IO ()
main = parseAndShow "profiles.csv"
