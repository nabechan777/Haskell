{- |
Module: Coproduct
Description: Category theory in Haskell
Copyright: Copyright 2018 DaikiWatanabe

    余積の圏
        ・余積は和ともいう。
        ・加算と関係がある。
-}
module Coproduct where

import Data.Void

-- | 結合律を表すためのデータ型
data Triple a c b = Left' a | Middle' c | Right' b deriving (Eq, Show, Read)

-- | 交換律
sswap :: Either a b -> Either b a
sswap (Left x) = Right x
sswap (Right x) = Left x

-- | 結合律
sassoc :: Either a (Either c b) -> Triple a c b
sassoc (Left x) = Left' x
sassoc (Right (Left x)) = Middle' x
sassoc (Right (Right x)) = Right' x

sassocInv :: Either (Either a c) b -> Triple a c b
sassocInv (Left (Left x)) = Left' x
sassocInv (Left (Right x)) = Middle' x
sassocInv (Right x) = Right' x

-- | ゼロ
mzero :: Either a Void -> a
mzero = undefined

mzeroInv :: a -> Either a Void
mzeroInv = undefined
