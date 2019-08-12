{- |
Module: Coproduct
Description: Category theory in Haskell
Copyright: Copyright 2018 DaikiWatanabe

    積・余積の圏
-}
module ProductAndCoproduct where

import Product
import Coproduct

-- | 分配律
distr :: (a, Either b c) -> Either (a, b) (a, c)
distr (x, Left y) = Left (x, y)
distr (x, Right y) = Right (x, y)
