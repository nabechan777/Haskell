module Sample1 where

import Data.Char
import Data.Maybe
import Control.Monad.State

operation :: (Num a, Eq a, Fractional a) => String -> a -> a -> Maybe a
operation "+" x y = Just (x + y)
operation "-" x y = Just (x - y)
operation "*" x y = Just (x * y)
operation "/" x y
    | y == 0 = Nothing
    | otherwise = Just (x / y)

calculation :: [String] -> State [Maybe Double] ()
calculation [] = get >>= put
calculation (x:xs) = do
    if isOperator x
        then do
            (b:a:bs) <- get
            let a' = fromJust a
                b' = fromJust b
                res = operation x a' b'
            put (res:bs)
        else do
            bs <- get
            let x' = Just (read x :: Double)
            put (x':bs)
    calculation xs
    where
        isOperator :: String -> Bool
        isOperator "+" = True
        isOperator "-" = True
        isOperator "*" = True
        isOperator "/" = True
        isOperator _ = False

run :: IO ()
run = do
    putStrLn "Input expression.(etc 10 2 + 3 * 7 10 + /)"
    input <- fmap words getLine
    let result = execState (calculation input) []
    mapM_ print result
