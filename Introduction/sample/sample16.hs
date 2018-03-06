{-
    関手
    ・fmap :: (a -> b) -> t a -> t b
    ・fmapを実装しているインスタンスを関手という。

    モナド
    ・(>>=) :: m a -> (a -> m b) -> m b
    ・return :: a -> m a
    ・(>>=)とreturnを実装しているインスタンスをモナドと呼ぶ。

    do式
    ・モナド式/パターン <- モナド式/let宣言が使える。
    ・最後の式の評価がdo式全体の評価となる。
    ・よってdo式内の最後の式はモナド式でないといけない。
-}

div2 :: Int -> Maybe Int
div2 x = if even x then Just (x `div` 2) else Nothing

div8 :: Int -> Maybe Int
div8 x = return x >>= div2 >>= div2 >>= div2

div8' :: Int -> Maybe Int
div8' x = do
    y <- div2 x
    z <- div2 y
    div2 z

main = do
    print $ div8 32
    print $ div8 50
    print $ div8' 32
    print $ div8' 50
