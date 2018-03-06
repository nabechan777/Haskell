module EuclidImpl
    ( RunType (..)
    , findGreatestCommonFacter
    , benchMarkList
    ) where

import Control.Monad.Writer

data RunType = Type1 | Type2 deriving (Show)
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    mappend (DiffList f) (DiffList g) = DiffList (\xs -> f (g xs))


toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        let b' = a `mod` b
        tell [show a ++ " mod " ++ show b ++ " = " ++ show b']
        gcd' b b'

refactoringGCD :: Int -> Int -> Writer (DiffList String) Int
refactoringGCD a b
    | b == 0 = do
        tell (toDiffList ["Finshed with " ++ show a])
        return a
    | otherwise = do
        let b' = a `mod` b
        result <- refactoringGCD b b'
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show b'])
        return result

findGreatestCommonFacter :: RunType -> Int -> Int -> (Int, [String])
findGreatestCommonFacter Type1 a b = runWriter $ gcd' a b
findGreatestCommonFacter Type2 a b = (greaterCommonFacter, diffList)
    where
        result = runWriter $ refactoringGCD a b
        greaterCommonFacter = fst result
        diffList = fromDiffList (snd result)

benchMarkDiffList :: Int -> Writer (DiffList String) ()
benchMarkDiffList 0 = do
    tell (toDiffList ["0"])
benchMarkDiffList x = do
    benchMarkDiffList (x - 1)
    tell (toDiffList [show x])

benchMarkDefaultList :: Int -> Writer [String] ()
benchMarkDefaultList 0 = do
    tell ["0"]
benchMarkDefaultList x = do
    benchMarkDefaultList (x - 1)
    tell [show x]

benchMarkList :: RunType -> IO ()
benchMarkList Type1 = mapM_ putStrLn . fromDiffList . snd . runWriter $ benchMarkDiffList 100000
benchMarkList Type2 = mapM_ putStrLn . snd . runWriter $ benchMarkDefaultList 100000
