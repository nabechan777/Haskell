module RopeWalkingByEitherImpl where


type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right)
    | result < 4 = Right (newLeft, right)
    | otherwise = Left $ "Pierre fell off the rope: " ++ show (newLeft, right)
    where
        newLeft = left + n
        result = newLeft - right

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right)
    | result < 4 = Right (left, newRight)
    | otherwise = Left $ "Pierre fell off the rope: " ++ show (left, newRight)
    where
        newRight = right + n
        result = newRight - left

banana :: Pole -> Either String Pole
banana _ = Left "Pierre stepped on the banana and fell down."
