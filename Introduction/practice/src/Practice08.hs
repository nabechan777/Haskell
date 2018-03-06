{-
    練習問題（ラムダ）
-}

module Practice08
    ( result_practice08
    ) where

fact :: Int -> Int
fact = \x -> case x of
    0 -> 1
    _ | x > 0 -> x * fact (x - 1)

add :: Int -> Int -> Int
add = \x y -> x + y

combine :: Int -> Int -> Int -> [Int]
combine = \x -> \y -> \z -> x:y:[z]

mymap :: (Int -> Int) -> [Int] -> [Int]
mymap g xs = [g x | x <- xs]

map' :: (Int -> Int) -> [Int] -> [Int]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = if f x then x : filter' f xs else filter' f xs

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

foldl' :: (Int -> Int -> Int) -> Int -> [Int] -> Int
foldl' _ s [] = s
foldl' f s (x:xs) = foldl' f v xs
    where
        v = f s x

foldr' :: (Int -> Int -> Int) -> Int -> [Int] -> Int
foldr' _ s [] = s
foldr' f s (x:xs) = f x $ foldr' f s xs

-- reverse' :: (Num a, Eq a, Ord a) => [a] -> [a]
reverse' = foldl (flip (:)) []

maximum' :: (Num a, Eq a, Ord a) => [a] -> a
maximum' (x:xs) = foldl max x xs

minmum' :: (Num a, Eq a, Ord a) => [a] -> a
minmum' (x:xs) = foldl min x xs

qsortByFilter :: (Eq a, Ord a) => [a] -> [a]
qsortByFilter [] = []
qsortByFilter (x:xs) = qsortByFilter lt ++ [x] ++ qsortByFilter ge
    where
        lt = filter (< x) xs
        ge = filter (>= x) xs

bswap :: (Eq a, Ord a) => [a] -> [a]
bswap = foldr (\x xs -> case xs of
    [] -> [x]
    (y:ys) | x > y -> y:x:ys
           | otherwise -> x:y:ys ) []

-- Yコンビネータを用いることで無名関数の再帰が実現できる。
-- Yコンビネータとは関数fを受け取り、結果としてfの引数としてYコンビネータにfを渡した時の処理
exsampleYCombinater :: Int
exsampleYCombinater =
    flip y [1..100] $
        \sum xs -> case xs of
            [] -> 0
            (x:xs') -> x + sum xs'
    where
        y f = f (y f)

fibonacciByYCombinater :: Int -> Int
fibonacciByYCombinater x =
    flip y x $
        \fib n -> case n of
            0 -> 0
            1 -> 1
            otherwise -> fib (n - 1) + fib (n - 2)
    where
        y f = f (y f)

result_practice08 :: IO ()
result_practice08 = do
    putStrLn "description:\n\tExsample of Lambda\n"
    putStr "fact 10 = "
    print $ fact 10
    putStr "add 7 7 = "
    print $ add 7 7
    putStr "(\\x y -> x + y) 7 7 = "
    print $ (\x y -> x + y) 7 7
    putStr "combine 1 2 3 = "
    print $ combine 1 2 3
    putStr "mymap (* 2) [0..9] = "
    print $ mymap (* 2) [0..9]
    putStr "(1 +) 2 = "
    print $ (1 +) 2
    putStr "map' (* 2) [1..5] = "
    print $ map' (* 2) [1..5]
    putStr "filter' (< 5) [1..5] = "
    print $ filter (< 5) [1..5]
    putStr "foldl' (-) 0 [1..5] = "
    print $ foldl' (-) 0 [1..5]
    putStr "foldr' (-) 0 [1..5] = "
    print $ foldr' (-) 0 [1..5]
    putStr "reverse' [1..5] = "
    print $ reverse' [1..5]
    putStr "maximum' [1..5] = "
    print $ maximum' [1..5]
    putStr "minmum' [1..5] = "
    print $ minmum' [1..5]
    putStr "qsortByFilter [4, 6, 9, 8, 3, 5, 1, 7, 2] = "
    print $ qsortByFilter [4, 6, 9, 8, 3, 5, 1, 7, 2]
    putStr "bswap [4, 3, 1, 5, 2] = "
    print $ bswap [4, 3, 1, 5, 2]
    putStr "exsampleYCombinater = "
    print $ exsampleYCombinater
    putStr "fibonacciByYCombinater 10 = "
    print $ fibonacciByYCombinater 10
