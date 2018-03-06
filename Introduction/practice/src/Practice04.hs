{-
    練習問題(ソート)
-}

module Practice04
    ( result_practice04
    ) where

import Debug.Trace

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
    | x < y = x : y : ys
    | otherwise = y : insert x ys

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert x $ isort xs

isort' :: [Int] -> [Int]
isort' [] = []
isort' (x:xs) = trace dbg1 $ trace dbg2 ret
    where
        ret = insert x xs'
        xs' = isort' xs
        dbg1 = "isort " ++ show (x:xs) ++ " = " ++ "insert " ++ show x ++ "(isort " ++ show xs ++ ")"
        dbg2 = "insert " ++ show x ++ " " ++ show xs' ++ " = " ++ show ret


bswap :: [Int] -> [Int]
bswap [x] = [x]
bswap (x:xs)
    | x > y = y:x:ys
    | otherwise = x:y:ys
    where
        (y:ys) = bswap xs

bsort :: [Int] -> [Int]
bsort [] = []
bsort xs = y : bsort ys
    where
        (y:ys) = bswap xs

bswap' :: [Int] -> [Int]
bswap' [x] = [x]
bswap' (x:y:zs)
    | x > y = y : x : bswap' zs
    | otherwise = x : y : bswap' zs

bsort' :: [Int] -> [Int]
bsort' [] = []
bsort' xs = y : bsort ys
    where
        (y:ys) = bswap xs

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort $ take h xs) (msort $ drop h xs)
    where
        h = (length xs) `div` 2

qsort :: [Int] -> [Int]
qsort [] = []
qsort (p:xs) = qsort smaller ++ [p] ++ qsort larger
    where
        smaller = [x | x <- xs, x < p]
        larger = [x | x <- xs, x >= p]

result_practice04 :: IO ()
result_practice04 = do
    let list = [2,4,2,1,5,7,5,3,7,2,1,9]
    putStrLn "description:\n\tDefine three kind of sort functions.\n"
    putStr "list = "
    print list
    putStr "isort list = "
    print $ isort list
    putStr "bsort list = "
    print $ bsort list
    putStr "msort list = "
    print $ msort list
    putStr "qsort list = "
    print $ qsort list

main = do
    let list = [2,4,2,1,5,7,5,3,7,2,1,9]
    print $ isort list
    -- print $ isort' list
    print $ bsort list
    -- print $ bsort' list
    print $ msort list
    print $ qsort list
