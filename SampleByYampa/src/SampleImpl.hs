{-# LANGUAGE Arrows #-}
module SampleImpl where

import FRP.Yampa

cooling :: Double -> SF () Double
-- cooling t0 = (constant (-1) >>> integral) >>^ (+ t0)
cooling t0 = proc input -> do
    t0' <- integral >>^ (+ t0) -< -1
    returnA -< t0'

coolingWithFloor :: Double -> SF () Double
coolingWithFloor t0 = switch cooling' atRoomTemp
    where
        cooling' :: SF () (Double, Event Double)
        cooling' = proc _ -> do
            t' <- cooling t0 -< ()
            e <- edge -< t' <= 18
            returnA -< (t', e `tag` t')
        atRoomTemp :: Double -> SF () Double
        atRoomTemp _ = constant 18

run :: IO ()
run = do
    let sensorData = (80.0, zip (cycle [60.0]) [Just 78.6, Just 68.9, Just 61.5])
    print $ embed (constant 77) sensorData
    print $ embed identity sensorData
    print $ embed time sensorData
    print $ embed (cooling (80.0 :: Double)) ((), take 20 $ cycle [(1.0, Nothing)])
    print $ embed (coolingWithFloor 25) ((), take 10 $ cycle [(1.0, Just ())])
    reactimate (return ())
               (\_ -> return (1.0, Nothing))
               (\_ b -> (putStrLn $ show b) >> if b <= 18.0 then return True else return False)
               (coolingWithFloor 25.0)
