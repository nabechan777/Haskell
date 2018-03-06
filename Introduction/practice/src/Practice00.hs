{-
    練習問題（パターンマッチング、ガード、case文）
-}

module Practice00
                ( result_practice00
                ) where

-- パターンマッチング
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- ガード
fib' :: Int -> Int
fib' n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fib' (n - 1) + fib' (n - 2)

-- case式
fib'' :: Int -> Int
fib'' n = case n of
    0 -> 0
    1 -> 1
    _ | n > 0 -> fib'' (n - 1) + fib'' (n - 2)

result_practice00 :: IO ()
result_practice00 = do
    putStrLn "description:\n\tThree pattern functions to caluculation \'Fibnacci Number\'\n"
    putStr "map (fib_pattern_matching [0..10]) = "
    print $ map fib [0..10]
    putStr "map (fib_gurd [0..10]) = "
    print $ map fib' [0..10]
    putStr "map (fib_case [0..10]) = "
    print $ map fib'' [0..10]

main = do
    print $ map fib [0..10]
    print $ map fib' [0..10]
    print $ map fib'' [0..10]
