{-
    cons演算子
    ・(:) :: a -> [a] -> [a] <既存のリストの先頭に1つ要素を追加する演算子>

    リストを扱う関数
    ・map :: (a -> b) -> [a] -> [b] <各リストの要素に対して関数を適用したリストとして返す>
    ・(!!) :: [a] -> Int -> a <任意のリストの要素を返す>
    ・take :: Int -> [a] -> [a] <任意の長さの部分リストを返す>
    ・(++) :: [a] -> [a] -> [a] <リストの連結>
    ・concat :: [[a]] -> [a] <リストの平坦化>
    ・zip :: [a] -> [b] -> [(a, b)] <各リストの値で構成されるタプルのリストを返す>
    ・zipWith :: (a -> b -> c) -> [a] -> [b] ->[c] <2つのリストを関数で合成する>

    リストの畳み込み
    ・foldr :: (a -> b -> b) -> b -> [a] -> b <右から畳み込む>
    ・foldl :: (a -> b -> a) -> a -> [b] -> a <左から畳み込む>

    数列表記
    ・[m..n] = [m, m+1, ..., n-1, n]
    ・[m..] = 無限リスト＜遅延評価のためプログラムが終了する＞

    内包表記
    ・[式 | 限定子1, 限定子2, ...]
    ・限定子は、生成器/ガード/局所宣言の3つがある。
    ・生成器：パターン <- 式
    ・ガード：条件式
    ・局所宣言：letで始まる宣言
-}

list_value1 = [1,2,3]
list_value2 = 1 : (2 : (3 : []))
list_value3 = [0,1,2,3,4,5,6,7,8,9]
list_value4 = [0..9]
list_value5 = [x | x <- [1..100], odd x]
list_value6 = [(x, y) | x <- [1, 2, 3], let y = 4]
list_value7 = [2,4,1,6,8,3,5,11,6,3,7,9,7,2,10]
list_value8 = [0..]

qsort :: [Int] -> [Int]
qsort [] = []
qsort (p:xs) = qsort smaller ++ [p] ++ qsort larger
    where
        smaller = [x | x <- xs, x < p]
        larger = [x | x <- xs, x >= p]

main = do
    print list_value1
    print list_value2
    print list_value3
    print list_value4
    print list_value5
    print list_value6
    print $ take 2 list_value8
    print $ map (2 *) list_value3
    print $ list_value3 !! 3
    print $ take 3 list_value3
    print $ list_value1 ++ list_value2
    print $ zip list_value1 list_value2
    print $ foldr (+) 0 list_value3
    print $ foldl (-) 0 list_value3
    print list_value7
    print $ qsort list_value7
