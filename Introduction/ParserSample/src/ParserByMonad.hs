
module ParserByMonad where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.State
import Data.Char

parseTest :: (Show a, Show a1) => StateT a1 (Either ([Char], t)) a -> a1 -> IO ()
parseTest p s =
    case evalStateT p s of
        Right r -> print r
        Left (e, _) -> putStrLn $ "[" ++ show s ++ "] " ++ e

anyChar :: StateT [a] (Either ([Char], [a])) a
anyChar = StateT $ anyChar
    where
        anyChar (x:xs) = Right (x, xs)
        anyChar xs = Left ("too short", xs)

satisfy :: Show a => (a -> Bool) -> StateT [a] (Either ([Char], [a])) a
satisfy f = StateT $ satisfy
    where
        satisfy (x:xs) | not $ f x = Left (": " ++ show x, x:xs)
        satisfy xs = runStateT anyChar xs

(StateT a) <|> (StateT b) = StateT f where
    f s0 = (a s0) <|> (b s0) where
        Left (a, s1) <|> _ | s0 /= s1 = Left (a, s1)
        Left (a, _) <|> Left (b, s2) = Left (b ++ a, s2)
        Left _ <|> b = b
        a <|> _ = a

left :: t1 -> StateT t (Either (t1, t)) a
left e = StateT $ \s -> Left (e, s)

char :: (Eq a, Show a) => a -> StateT [a] (Either ([Char], [a])) a
char c = satisfy (== c) <|> left ("not char " ++ show c)

digit :: StateT [Char] (Either ([Char], [Char])) Char
digit = satisfy isDigit <|> left "not digit"

letter :: StateT [Char] (Either ([Char], [Char])) Char
letter = satisfy isLetter <|> left "not letter"

myTry :: StateT t1 (Either (t, t1)) a -> StateT t1 (Either (t, t1)) a
myTry (StateT p) = StateT $ \s -> case p s of
    Left (e, _) -> Left (e, s)
    r -> r

myString :: String -> StateT String (Either ([Char], String)) String
myString s = sequence [char x | x <- s]


many :: Eq t => StateT t (Either ([a], t)) a1 -> StateT t (Either ([a], t)) [a1]
many p = ((:) <$> p <*> many p) <|> return []

test5 :: StateT [Char] (Either ([Char], [Char])) [Char]
test5 = sequence [letter, digit, digit, digit]

test6 :: StateT [Char] (Either ([Char], [Char])) [Char]
test6 = sequence $ letter : replicate 3 digit

test7 :: StateT [Char] (Either ([Char], [Char])) [Char]
test7 = many letter

test8 :: StateT [Char] (Either ([Char], [Char])) [Char]
test8 = many (letter <|> digit)

main :: IO ()
main = do
    parseTest test5 "a123"
    parseTest test5 "ab123"
    parseTest test6 "a123"
    parseTest test6 "ab123"
    parseTest test7 "abc123"
    parseTest test7 "123abc"
    parseTest test8 "abc123"
    parseTest test8 "123abc"
