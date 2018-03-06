{-
    関数定義
    ・関数名 引数1 引数2 ... 引数n = 定義
    ・引数型1 -> 引数型2 -> ... -> 引数型n -> 返値型
    ・ループ文がないため再帰を利用する。

    パターンマッチング
    ・関数定義が複数ある場合、引数のパターンが最初に一致する定義が適用される。

    if式
    ・文ではなく式なので、値を持つ。
    ・よってthen節とelse節のどちらとも省略することができない。

    局所定義
    ・let{ 宣言1; 宣言2; ... 宣言n; } in 式
    ・変数や補助関数を局所的に定義することができる。
    ・where { 宣言1; 宣言2; ... 宣言n; }
    ・whereを使うことでも定義することができる。

    中置演算子
    ・()で囲むことで関数と同様の扱いができる。
    ・逆に2引数の関数を``で囲むことで中置記法ができる。
    ・関数適用の優先順位は中置演算子よりも高い。
-}

add :: Int -> Int -> Int
add x y = x + y

fact :: Int -> Int
fact 0 = 1
fact x = x * fact (x - 1)

absolute :: Int -> Int
absolute x = if x < 0 then -x else x

cylinder :: Double -> Double -> Double
cylinder r h =
    let
        square x = x * x
        base = 3.14 * square r
    in
        h * base

cylinder' :: Double -> Double -> Double
cylinder' r h = h * base
    where
        square x = x * x
        base = 3.14 * square r

main = do
    print $ add 2 3
    print $ fact 5
    print $ absolute (-9)
    print $ cylinder 2.0 5.0
    print $ cylinder' 2.0 5.0
    print $ (+) 3 2
    print $ 8 `div` 2
