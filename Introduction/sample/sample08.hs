{-
    ワイルドカード
    ・ワイルドカード_は値を無視する際に用いる。
    ガード
    ・定義の適用を条件式で分岐することができる。

    case文
    ・式としてパターンマッチングを行うことができる。
    ・case 式 of { 選択肢1; 選択肢2; ...; 選択肢n }
    ・パターン->式[where宣言列], パターン | ガード1->式1 | ガード2->式2 | ... | ガードm->式m [where宣言列]
    ・タプルに対するパターンは(x, y)の形で可能
    ・リストに対するパターンはx:xsをつかうことで、リストの先頭とリスト残りを取り出すことができる。
-}

power :: Int -> Double -> Double
power 0 _ = 1.0
power x y | x > 0 = y * power (x - 1) y
          | otherwise = 1.0 / power (- x) y

power' :: Int -> Double -> Double
power' x y = case (x, y) of
    (0, _) -> 1.0
    (x, y) | x > 0 -> y * power (x - 1) y
           | otherwise -> 1.0 / power (- x) y

prod :: [Int] -> Int
prod [] = 1
prod (x:xs) = x * prod xs

trim :: [Char] -> [Char]
trim str@(c:cs)
    | c == '0' = trim cs
    | otherwise = str

main = do
    print $ power 8 2.0
    print $ power (-3) 2.0
    print $ power' 8 2.0
    print $ power' (-3) 2.0
    print $ prod [1, 2 ,3]
    print $ trim "000000123"
