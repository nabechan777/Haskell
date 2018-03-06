module RopeWalkingImpl
    ( SimulationType (..)
    , simulationRopeWalking
    ) where


type Birds = Int
type Pole = (Birds, Birds)
data SimulationType = Type1 | Type2 deriving (Show)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs (newLeft - right) < 4 = Just (newLeft, right)
    | otherwise = Nothing
    where
        newLeft = left + n

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (newRight - left) < 4 = Just (left, newRight)
    | otherwise = Nothing
    where
        newRight = right + n

simulationRopeWalking :: SimulationType -> Pole -> Maybe Pole
simulationRopeWalking Type1 pole = return pole >>= landLeft 2 >>= landRight 2 >>= landRight 2
simulationRopeWalking Type2 pole = return pole >>= landLeft 3 >>= landRight 2 >>= landRight 3 >>= landLeft (-2) >>= landLeft 1
