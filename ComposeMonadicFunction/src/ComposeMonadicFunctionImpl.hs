module ComposeMonadicFunctionImpl
    ( composeFunction
    , composeMonadicFunction
    ) where

import Control.Monad

composeFunction :: Int -> Int
composeFunction = foldr (.) id [(+8),(*100),(+1)]

composeMonadicFunction :: Int -> Maybe Int
composeMonadicFunction = foldr (<=<) return [f,g,h]
    where
        f = (\x -> Just (x + 8))
        g = (\x -> Just (x * 100))
        h = (\x -> Just (x + 1))
