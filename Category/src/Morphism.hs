{- |
Module: Introduction
Description: Category theory on Haskell
Copyright: Copyright 2018 DaikiWatanabe

    個数mのドメインから個数nのコドメインの時、異なる射の数はn^m個ある。

    例；
        ドメイン：要素数 = 3, [a1, a2, a3]
        コドメイン：要素数 = 2, [b1, b2]

        ドメインからコドメインへの射の数をもとめる。
        -> ドメインの要素をコドメインの要素を対応させる。
            a3 -> [b1, b2]
        -> この操作を再帰的に繰り返す。
            a2 -> (a3 -> [b1, b2]) = a2 -> [b1, b2]
            a1 -> (a2 -> [b1, b2]) = a1 -> [[b1, b1], [b1, b2], [b2, b1], [b2, b2]]


    ドメインの要素数が1の時、射の数は1である。また0の時、射の数も0である。

-}

module Morphism where

-- | 対応づけされたリスト（ドメインからコドメインへの射）からドメインの次の要素を対応づけしたリストを返す。
addRep :: [b] -> [[b]] -> [[b]]
addRep [] _ = []
addRep _ [] = []
addRep (y:ys) (l:ls) = (y:l) : addRep [y] ls ++ addRep ys (l:ls)

-- | ドメインの左端から右端にコドメインの要素に対応づけをする。（ドメインからコドメインへの異なる射を求める）
morphism_ :: [a] -> [b] -> [[b]]
morphism_ _ [] = []
morphism_ [] ys = [ys]
morphism_ (x:[]) (y:ys) = [y] : morphism_ (x:[]) ys
morphism_ (x:xs) ys = addRep ys $ morphism_ xs ys

-- | ドメインの要素数とコドメインの要素数を受け取って異なる射のリストを返す。
morphism :: (Num b, Num a, Enum b, Enum a) => a -> b -> [[b]]
morphism m n = morphism_ [1..m] [1..n]

-- | コドメインの要素数ごとに関数を定義する。
morphism0 :: (Num a, Enum a) => a -> [[a]]
morphism0 n = morphism 0 n

morphism1 :: (Num a, Enum a) => a -> [a]
morphism1 n = toPair1 $ morphism_ [1..1] [1..n]
    where
        toPair1 [] = []
        toPair1 (f:fs) = (f !! 0) : toPair1 fs

morphism2 :: (Num a, Enum a) => a -> [(a, a)]
morphism2 n = toPair2 $ morphism_ [1..2] [1..n]
    where
        toPair2 [] = []
        toPair2 (f:fs) = (f !! 0, f !! 1) : toPair2 fs

morphism3 :: (Num a, Enum a) => a -> [(a, a, a)]
morphism3 n = toPair3 $ morphism_ [1..3] [1..n]
    where
        toPair3 [] = []
        toPair3 (f:fs) = (f !! 0, f !! 1, f !! 2) : toPair3 fs

morphism4 :: (Num a, Enum a) => a -> [(a, a, a, a)]
morphism4 n = toPair4 $ morphism_ [1..4] [1..n]
    where
        toPair4 [] = []
        toPair4 (f:fs) = (f !! 0, f !! 1, f !! 2, f !! 3) : toPair4 fs

morphism5 :: (Num a, Enum a) => a -> [(a, a, a, a, a)]
morphism5 n = toPair5 $ morphism_ [1..5] [1..n]
    where
        toPair5 [] = []
        toPair5 (f:fs) = (f !! 0, f !! 1, f !! 2, f !! 3, f !! 4) : toPair5 fs

morphism6 :: (Num a, Enum a) => a -> [(a, a, a, a, a, a)]
morphism6 n = toPair6 $ morphism_ [1..6] [1..n]
    where
        toPair6 [] = []
        toPair6 (f:fs) = (f !! 0, f !! 1, f !! 2, f !! 3, f !! 4, f !! 5) : toPair6 fs
