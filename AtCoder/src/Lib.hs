module Lib where

import Data.Char

ratedForMe :: Int -> String
ratedForMe r
    | r < 1200 = "ABC"
    | r < 2800 = "ARC"
    | otherwise = "AGC"

acCepted1 :: String -> Maybe String
acCepted1 ('A':xs) = (Just xs)
acCepted1 _ = Nothing

acCepted2 :: String -> Maybe String
acCepted2 xs = if f then Just (filter isLower xs) else Nothing
    where
        f = foldr xor False $ map (\x -> if x == 'C' then True else False) xs

acCapted3 :: Maybe String -> String
acCapted3 (Just _) = "AC"
acCapted3 Nothing = "WA"

nand :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = True

xor :: Bool -> Bool -> Bool
xor x y =  (x || y) && (nand x y)
