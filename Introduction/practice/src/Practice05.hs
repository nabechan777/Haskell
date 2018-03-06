{-
    練習問題（リストの内包的記法）
-}

module Practice05
    ( result_practice05
    ) where

--三辺の長さが引数で指定された長さ以下の直角三角形
rightTrianglePoint :: Int -> [(Int, Int, Int)]
rightTrianglePoint l = [(x, y, z) | x <- [1..l], y <- [x..l], z <- [y..l], x * x + y * y == z * z]

result_practice05 :: IO ()
result_practice05 = do
    putStrLn "description:\n\tDefine function using list inclusive notation.\n"
    putStr "rightTrianglePoint 20 = "
    print $ rightTrianglePoint 20

main = do
    -- 重腹の排除
    -- print [(a, b, c) | a <- [1..20], b <- [a..20], c <- [b..20], a * a + b * b == c * c]
    print $ rightTrianglePoint 20
    -- print $ rightTrianglePoint 20.0
