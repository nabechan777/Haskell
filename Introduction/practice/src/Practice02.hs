{-
    練習問題（タプル）
-}

module Practice02
    ( result_practice02
    )where

--直線と垂線との交点を求める関数
perpPoint :: (Double, Double) -> (Double, Double, Double) -> (Double, Double)
perpPoint (p, q) (a, b, c) = (x, y) where
    x = (a * c + b * d) / (a * a + b * b)
    y = (b * c - a * d) / (a * a + b * b)
    d = b * p - a * q

result_practice02 :: IO ()
result_practice02 = do
    putStrLn "description:\n\tDefine function to operate tuple. \n"
    putStr "perpPoint (0, 2) (1, -1, 0) = "
    print $ perpPoint (0, 2) (1, -1, 0)

main = do
    print $ perpPoint (0, 2) (1, -1, 0)
