
module ParserNothingMonad where

import Control.Exception
import Data.Char

parseTest :: Show a => (t -> (a, b)) -> t -> IO ()
parseTest p s = do
    print $ fst $ p s
    `catch` \(SomeException e) ->
        putStr $ show e

anyChar :: String -> (Char, String)
anyChar (x:xs) = (x, xs)

satisfy :: (Char -> Bool) -> String -> (Char, String)
satisfy f (x:xs) | f x = (x, xs)

char :: Char -> String -> (Char, String)
char x = satisfy (== x)

digit :: String -> (Char, String)
digit = satisfy isDigit

letter :: String -> (Char, String)
letter = satisfy isLetter

test1 :: String -> ([Char], String)
test1 xs0 = let
    (x1, xs1) = anyChar xs0
    (x2, xs2) = anyChar xs1
    in ([x1, x2], xs2)

test2 :: String -> ([Char], String)
test2 xs0 = let
    (x1, xs1) = test1 xs0
    (x2, xs2) = anyChar xs1
    in (x1 ++ [x2], xs2)

test3 :: String -> ([Char], String)
test3 xs0 = let
    (x1, xs1) = letter xs0
    (x2, xs2) = digit xs1
    (x3, xs3) = digit xs2
    in ([x1, x2, x3], xs3)

main :: IO ()
main = do
    parseTest anyChar "abc"
    parseTest test1   "abc"
    parseTest test2   "abc"
    parseTest test2   "12"      -- NG
    parseTest test2   "123"
    parseTest (char 'a') "abc"
    parseTest (char 'a') "123"  -- NG
    parseTest digit  "abc"      -- NG
    parseTest digit  "123"
    parseTest letter "abc"
    parseTest letter "123"      -- NG
    parseTest test3  "abc"      -- NG
    parseTest test3  "123"      -- NG
    parseTest test3  "a23"
    parseTest test3  "a234"
