{-
    練習問題（リスト操作関数の再実装）
-}

module Practice01
    ( result_practice01
    ) where

-- 要素の和
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- 要素の積
product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs

-- 部分リストの生成
take' :: Int -> [Int] -> [Int]
take' 0 _ = []
take' n (x:xs) = [x] ++ take' (n - 1) xs

-- 要素の切り落とし
drop' :: Int -> [Int] -> [Int]
drop' 0 x = x
drop' n (_:xs) = drop' (n - 1) xs

-- 逆順のリスト
reverse' :: [Int] -> [Int]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- 階乗の計算
fact :: Int -> Int
fact n = case n of
    0 -> 1
    _ | n > 0 -> product' [1..n]

result_practice01 :: IO ()
result_practice01 = do
    putStrLn "description:\n\tDefine functions to be operated by list.\n"
    putStr "sum [0..10] = "
    print $ sum' [0..10]
    putStr "product [0..10] = "
    print $ product' [1..10]
    putStr "take 3 [0..10] = "
    print $ take' 3 [0..10]
    putStr "drop 3 [0..10] = "
    print $ drop' 3 [0..10]
    putStr "reverse [0..10] = "
    print $ reverse' [0..10]
    putStr "fact 10 = "
    print $ fact 10

main = do
    print $ sum' [0..10]
    print $ product' [1..10]
    print $ take' 3 [0..10]
    print $ drop' 3 [0..10]
    print $ reverse' [0..10]
    print $ fact 10
