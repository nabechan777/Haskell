module ProbabilityImpl
    ( Prob (..)
    , monadLaw
    ) where

import Data.Ratio
import Control.Monad

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving (Show)

instance Functor Prob where
    fmap f xs = xs >>= return . f
    -- fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

instance Applicative Prob where
    pure = return
    (<*>) = ap

instance Monad Prob where
    return x = Prob [(x, 1%1)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where
        multAll (Prob innerxs, p) = map (\(x, r) -> (x, p * r)) innerxs

monadLaw :: IO ()
monadLaw = do
    putStrLn "\nMonad's First Law"
    let f = (\x -> Prob [(x, 1%2), ('b', 1%2)])
        g = (\x -> Prob [(x, 2%3), ('c', 1%3)])
        h = (\x -> Prob [(x, 1%5), ('d', 4%5)])
        compose1 = (f <=< g) <=< h
        compose2 = f <=< (g <=< h)
    putStr "return m >>= f = " >> return (return 'a' >>= f) >>= print
    putStr "f x = " >> return (f 'a') >>= print
    putStrLn "\nMonad's Second Law"
    putStr "m >>= return = " >> return (Prob [('a', 1%2)] >>= return) >>= print
    putStr "m = " >> return (Prob [('a', 1%2)]) >>= print
    putStrLn "\nMonad's Third Law"
    putStr "(f <=< g) <=< h = " >> return (return 'a' >>= compose1) >>= print
    putStr "f <=< (g <=< h) = " >> return (return 'a' >>= compose2) >>= print
