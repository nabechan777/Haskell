{-# LANGUAGE Arrows #-}
module SampleImpl where

import Control.Arrow

add :: Double -> Double
add = arr (\x -> (x, x)) >>> first (* 7) >>> ((+ 10) *** (/ 4)) >>> arr (uncurry (+))
