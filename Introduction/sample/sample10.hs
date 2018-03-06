{-
    代数的データ型
    ・data宣言で定義されたデータ型
    ・型構成子とデータ構成子は大文字で始まらなければならない。
    ・データ構成子は複数定義することができる。

    多層型
    ・型引数を持つ代数的データ型
    ・型引数：実行時に具体的な型に置き換えられる。

    再帰型
    ・データ型を再帰的に定義することができる。

    __構文__
    data 型構成子 型引数1 型引数2 ... =
        データ構成子1 フィールド11 フィールド12 ... |
        データ構成子2 フィールド21 フィールド22 ... | ...

    型シノニム
    ・type 型構成子 型引数1 型引数2 ... 型引数n = 元の型
    ・同一に扱うことができる。

    newtype宣言
    ・既存の型をラップした型を定義する。
    ・データ構成子数1, フィールド1のdata宣言と同様であるが軽い。(元の型と同じ内部表現で扱われるため)

    中置データ構成子
    ・":"で始まる演算子をデータ構成子として使える。

    フィールドラベル
    ・フィールドに名前をつけることができる。(data宣言, newtype宣言で可能)
-}

data Point = Pt2D Double Double | Pt3D Double Double Double

norm :: Point -> Double
norm (Pt2D x y) = sqrt (x * x + y * y)
norm (Pt3D x y z) = sqrt (x * x + y * y + z * z)

data Tuple a b = T a b
first :: Tuple a b -> a
first (T x _) = x

second :: Tuple a b -> b
second (T _ y) = y


newtype DigitString = DigitStr String
atoi :: DigitString -> Int
atoi (DigitStr xs) = read xs


data List a = Cons a (List a) | Nil

length' :: List a -> Int
length' Nil = 0
length' (Cons a xs) = 1 + length' xs


data Parson = Parson { name :: String, age :: Int }

intro :: Parson -> String
-- intro (Parson n _) = "My name is " ++ n ++ "."
intro p = "My name is " ++ (name p) ++ "."

inc :: Parson -> Parson
inc p = let newage = (age p) + 1 in p{ age = newage }

parson1 = Parson "daiki" 22
parson2 = Parson { name = "taro", age = 20 }

main = do
    print $ norm (Pt2D 3 0)
    print $ norm (Pt2D 1 2)
    print $ norm (Pt3D 0 1 2)
    print $ first (T 1 'a')
    print $ second (T 518 "Daiki")
    print $ length' Nil
    print $ length' (Cons 1 (Cons 2 (Cons 3 Nil)))
    print $ atoi (DigitStr "000123")
    print $ name parson1
    print $ age parson2
    print $ intro parson1
    print $ age $ inc parson2
