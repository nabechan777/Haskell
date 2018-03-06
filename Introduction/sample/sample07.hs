{-
    高階関数
    ・関数を引数や返値として扱う関数
    ・map関数は引数に関数とリストをとり、リストの各要素に関数を適用した結果のリストを返す。
    ・id関数は引数をそのまま返す関数。

    ラムダ抽象
    ・\ 引数1 引数2 ... 引数n -> 定義

    部分適用
    ・引数の一部を適用したものも関数である。
    ・部分適用が可能な関数をカリー化されている関数という。

    中置演算子の部分適用（セクション）
    ・(x +) = \y -> x + y
    ・(+ y) = \x -> x + y
    ・(+) = \x y -> x + y
    ・(- x)は単項演算子として解釈されるので、(subtract x)または(+ (-x))とする。

    関数合成
    ・foo x = f (g (h x))
    ・foo x = (f . g . h) x
    ・foo = f . g . h
-}

multiply :: Int -> Int -> Int
multiply x y = x * y

double :: Int -> Int
double = multiply 2

square :: Int -> Int
square x = (*) x x

--高階関数の定義（Intの引数と返値を持つ関数とIntの値を受け取り、Intを返す高階関数）
summation :: (Int -> Int) -> Int -> Int
summation f 1 = f 1
summation f n = f n + summation f (n - 1)

list_value = [1,2,3,4,5,6,7,8,9]

main = do
    print $ map id list_value
    print $ map double list_value
    print $ map (\ x -> x * 2) list_value
    print $ map square list_value
    print $ summation square 3
