module HanoiTower where

type Tower = ([Int], [Int], [Int])
type Move = Tower -> Tower


run :: IO ()
run = do
    (n:t:_) <- fmap (map read . words) getLine
    let result = (!! t) . hanoi ([1..n], [], []) $ (moveAtoC n)
        (a, b, c) = result
    printResult result

printResult :: Tower -> IO ()
printResult (a, b, c) = mapM_ printList [a, b, c]
    where
        printList xs
            | null xs = putStrLn "-"
            | otherwise = putStrLn $ (unwords . map show . reverse) $ xs

hanoi :: Tower -> [Move] -> [Tower]
hanoi = scanl (flip ($))

moveAtoB :: Int -> [Move]
moveAtoB n
    | n == 1 = [atb]
    | otherwise = moveAtoC (n-1) ++ moveAtoB 1 ++ moveCtoB (n-1)
    where
        atb (x:xs, ys, zs) = (xs, x:ys, zs)

moveAtoC :: Int -> [Move]
moveAtoC n
    | n == 1 = [atc]
    | otherwise = moveAtoB (n-1) ++ moveAtoC 1 ++ moveBtoC (n-1)
    where
        atc (x:xs, ys, zs) = (xs, ys, x:zs)

moveBtoA :: Int -> [Move]
moveBtoA n
    | n == 1 = [bta]
    | otherwise = moveBtoC (n-1) ++ moveBtoA 1 ++ moveCtoA (n-1)
    where
        bta (xs, y:ys, zs) = (y:xs, ys, zs)

moveBtoC :: Int -> [Move]
moveBtoC n
    | n == 1 = [btc]
    | otherwise = moveBtoA (n-1) ++ moveBtoC 1 ++ moveAtoC (n-1)
    where
        btc (xs, y:ys, zs) = (xs, ys, y:zs)

moveCtoA :: Int -> [Move]
moveCtoA n
    | n == 1 = [cta]
    | otherwise = moveCtoB (n-1) ++ moveCtoA 1 ++ moveBtoA (n-1)
    where
        cta (xs, ys, z:zs) = (z:xs, ys, zs)

moveCtoB :: Int -> [Move]
moveCtoB n
    | n == 1 = [ctb]
    | otherwise = moveCtoA (n-1) ++ moveCtoB 1 ++ moveAtoB (n-1)
    where
        ctb (xs, ys, z:zs) = (xs, z:ys, zs)
