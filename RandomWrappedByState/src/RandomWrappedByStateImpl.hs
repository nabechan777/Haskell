module RandomWrappedByStateImpl where

import System.Random
import Control.Monad.State

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
    a <- randomSt
    b <- randomSt
    c <- randomSt
    return (a, b, c)

randomAction :: Int -> IO (Bool, StdGen)
randomAction seed = return (runState randomSt (mkStdGen seed))
