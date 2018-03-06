{-
    型クラス制約(文脈)
    ・型クラスのインスタンスでなければならない
    ・型シグネチャででてくる。

    class宣言
    ・型クラスを定義する。
    ・クラスメソッドの定義が可能
    ・定義する際、型シグネチャは必ず必要、デフォルト実装は任意。

    instance宣言
    ・ある型クラスのインスタンスであることを示す。

    クラス拡張
    ・あるクラスの拡張として新しいクラスを定義する。
    ・継承
    ・class スーパークラス 型引数 => サブクラス 型引数 where
            クラスメソッドの型シグネチャ宣言
    ・多重継承も可能。

    deriving宣言
    ・クラスメソッドの実装が自明である時、instance宣言を省略することができる。
-}

data Point2D = Pt2D Double Double deriving (Eq, Ord)
-- instance Eq Point2D where
--     (Pt2D x y) == (Pt2D x' y') = x == x' && y == y'

-- add :: Num => a -> a
-- add x = x + x


data Point1D = Pt1D Double
instance Eq Point1D where
    (Pt1D x) == (Pt1D x') = x == x'
instance Ord Point1D where
    compare (Pt1D x) (Pt1D x')
        | x == x' = EQ
        | x < x' = LT
        | otherwise = GT

data Point3D = Pt3D Double Double Double deriving (Eq, Ord)

--高階クラス
class Containier c where
    cmap :: (a -> b) -> c a -> c b

instance Containier Maybe where
    -- cmap :: (a -> b) -> Maybe a -> Maybe b
    cmap f Nothing = Nothing
    cmap f (Just x) = Just (f x)

main = do
    -- print $ add 4
    -- print $ add 7.0
    print $ (Pt2D 1 0) == (Pt2D 1 0)
    print $ (Pt2D 0 1) == (Pt2D 1 0)
    print $ (Pt2D 2 3) /= (Pt2D 2 3)
    print $ (Pt2D 1 9) /= (Pt2D 8 7)
    print $ (Pt2D 1 9) >= (Pt2D 8 7)    
    print $ (Pt1D 1) < (Pt1D 2)
    print $ (Pt1D 1) > (Pt1D 2)
    print $ (Pt1D 3) <= (Pt1D 3)
    print $ (Pt1D 7) >= (Pt1D 5)
    print $ (Pt3D 1 2 3) == (Pt3D 1 2 3)
    print $ (Pt3D 7 8 9) <= (Pt3D 1 2 3)
    print $ cmap (2 *) Nothing
    print $ cmap (2 *) (Just 3)
