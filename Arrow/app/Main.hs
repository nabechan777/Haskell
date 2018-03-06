{-# LANGUAGE Arrows #-}

module Main where

import System.Random
import ArrowImpl

main :: IO ()
main = do
    rng <- getStdGen
    interact $ unlines
        . ("Welcome to Arrow Hangman":)
        . concat
        . map snd
        . takeWhile fst
        . runCircuit (hangman rng)
        . ("":)
        . lines
