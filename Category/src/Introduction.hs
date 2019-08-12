
{- |
Module: Introduction
Description: Category theory on Haskell
Copyright: Copyright 2018 DaikiWatanabe

　対象: データ型, 射: 関数
-}

module Introduction where

data Nat0 = Zero | Succ0 Nat0 deriving (Eq, Ord, Read, Show)
data Nat1 = One | Succ1 Nat1 deriving (Eq, Ord, Read, Show)

toNat0 :: Int -> Nat0
toNat0 m
    | m == 0 = Zero
    | m > 0 = Succ0 $ toNat0 (m - 1)
    | otherwise = error "Not Natural Number!!"

toNat1 :: Int -> Nat1
toNat1 m
    | m == 1 = One
    | m > 1 = Succ1 $ toNat1 (m - 1)
    | otherwise = error "Not Natural Number!!"


fromNat0 :: Nat0 -> Int
fromNat0 Zero = 0
fromNat0 (Succ0 n) = 1 + fromNat0 n

fromNat1 :: Nat1 -> Int
fromNat1 One = 1
fromNat1 (Succ1 n) = 1 + fromNat1 n

plus :: Nat1 -> Nat1 -> Nat1
m `plus` One = Succ1 m
m `plus` (Succ1 n) = Succ1 (m `plus` n)

times2 :: Nat1 -> Nat1
times2 m = m `plus` m


data Domain = D1 | D2 | D3 deriving (Read, Show)
data Codomain = CA | CB deriving (Read, Show)

morphism :: [((Domain, Codomain), (Domain, Codomain), (Domain, Codomain))]
morphism = [((D1, d1), (D2, d2), (D3, d3)) | d1 <- [CA, CB], d2 <- [CA, CB], d3 <- [CA, CB]]

round' :: Double -> Int
round' m
    | m >= 0 && m - fromIntegral (floor m) >= 0.5 = 1 + floor m
    | m >= 0 = floor m
    | m - fromIntegral (floor m) > 0.5 = 1 + floor m
    | otherwise = floor m

abs' :: Int -> Nat0
abs' m
    | m >= 0 = toNat0 m
    | otherwise = toNat0 (-m)

distance :: Double -> Nat0
distance = abs' . round'
