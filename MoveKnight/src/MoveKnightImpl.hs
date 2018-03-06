module MoveKnightImpl
    ( canReachIn3
    ) where

import Control.Monad

type VerticalAxis = Int
type HorizontalAxis = Int
type KnightPos = (VerticalAxis, HorizontalAxis)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (v, h) = do
    (v', h') <- [(v+2, h+1), (v+2, h-1), (v-2, h+1), (v-2, h-1), (v+1, h+2), (v+1, h-2), (v-1, h+2), (v-1, h-2)]
    guard (v' `elem` [1..8] && h' `elem` [1..8])
    return (v', h')

in3 :: KnightPos -> [KnightPos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start
