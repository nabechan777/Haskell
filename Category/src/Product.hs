{- |
Module: Product
Description: Category theory on Haskell
Copyright: Copyright 2018 DaikiWatanabe

    積の圏
-}
module Product where

import Data.Void

-- | 交換則
mswap :: (a, b) -> (b, a)
mswap p = (snd p, fst p)

-- | 結合律
massoc :: ((a, b), c) -> (a, (b, c))
massoc ((x, y), z) = (x, (y, z))

-- | 単位元
munit :: (a, ()) -> a
munit (x, _) = x

munitInv :: a -> (a, ())
munitInv x = (x, ())

-- | ゼロ
mzero :: (a, Void) -> Void
mzero = undefined
