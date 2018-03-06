module MonadicFunctionImpl where

import Control.Monad
import Control.Monad.Writer

keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large. throwing it away."]
        return False

powerset :: [a] -> [[a]]
powerset = filterM (\x -> [True, False])

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
    | x > 9 = Nothing
    | otherwise = Just (acc + x)
