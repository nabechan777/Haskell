{-
    練習問題（代数的データ型）
-}

module Practice06
    ( result_practice06
    ) where

import Data.Bits

-- 列挙型
data Color = Blue | Red | Green | White | Cyan | Yellow | Magenta deriving (Show, Enum, Eq)

mix :: Color -> Color -> Color
mix Blue Red = Magenta
mix Blue Magenta = Magenta
mix Blue Green = Cyan
mix Blue Cyan = Cyan
mix Blue Yellow = White
mix Red Magenta = Magenta
mix Red Green = Yellow
mix Red Cyan = White
mix Red Yellow = Yellow
mix Magenta Green = White
mix Magenta Yellow = White
mix Magenta Cyan = White
mix Green Cyan = Cyan
mix Green Yellow = Yellow
mix Cyan Yellow = White
mix White _ = White
mix c1 c2
    | c1 == c2 = c1
    | otherwise = mix c2 c1

-- 直積型
data Point = Point Double Double
data Rect = Rect Double Double Double Double

contains :: Rect -> Point -> Bool
contains (Rect x y width height) (Point px py) =
    (x <= px && px <= x + width) && (y <= py && py <= y + height)


-- 直和型
data Point' = Point' Int Int | Point3D Int Int Int
data Rect' = Rect' Int Int Int Int | Rect3D Int Int Int Int Int Int

contains' :: Rect' -> Point' -> Bool
contains' (Rect' x y width height) (Point' px py) =
    (x <= px && px <= x + width) && (y <= py && py <= y + height)
contains' (Rect3D x y z width height depth) (Point3D px py pz) =
    (x <= px && px <= x + width) && (y <= py && py <= y + height) && (z <= pz && z <= z + depth)

-- レコード構文
data Point'' = Point'' { px :: Int, py :: Int}
data Rect'' = Rect'' { x :: Int, y :: Int, width :: Int, height :: Int}

contains'' :: Rect'' -> Point'' -> Bool
contains'' rect point  =
    (px point <= x rect && px point <= (x rect) + (width rect)) && (py point <= y rect && py point <= (y rect) + (height rect))

result_practice06 :: IO ()
result_practice06 = do
    putStrLn "description:\n\tDefine data types and functions using algebraic data types.\n"
    putStrLn "<<<<<<<<<< Enumulation type >>>>>>>>>>"
    putStrLn "Color = Blue | Red | Green | White | Cyan | Yellow | Magenta deriving (Show, Enum, Eq)"
    putStrLn "mix Red Blue"
    print $ mix Red Blue
    putStrLn "\n<<<<<<<<<< Direct product type >>>>>>>>>>"
    putStrLn "Point = Double Double"
    putStrLn "Rect = Double Double Double Double"
    putStr "contains (Rect 2 2 3 3) (Point 2 2) = "
    print $ contains (Rect 2 2 3 3) (Point 2 2)
    putStrLn "\n<<<<<<<<<< Direct sum type >>>>>>>>>>"
    putStrLn "Point = Point Int Int | Point3D Int Int Int"
    putStrLn "Rect = Rect Int Int Int Int | Rect3D Int Int Int Int Int Int"
    putStr "contains (Rect3D 2 2 2 3 3 3) (Point3D 2 2 2) = "
    print $ contains' (Rect3D 2 2 2 3 3 3) (Point3D 2 2 2)
    putStrLn "\n<<<<<<<<<< Algebric data type using \"Recode Syntax\" >>>>>>>>>>"
    putStrLn "Point = Point {px :: Int, py :: Int}"
    putStrLn "Rect = Rect {x :: Int, y :: Int, width :: Int, height :: Int}"
    print $ contains'' (Rect'' 2 2 3 3) (Point'' 2 2)

main = do
    print $ mix Red Blue
    print $ contains (Rect 2 2 3 3) (Point 1 1)
    print $ contains (Rect 2 2 3 3) (Point 2 2)
    print $ contains' (Rect' 2 2 3 3) (Point' 2 2)
    print $ contains' (Rect3D 2 2 2 3 3 3) (Point3D 2 2 2)
    print $ contains'' (Rect'' 2 2 3 3) (Point'' 2 2)
