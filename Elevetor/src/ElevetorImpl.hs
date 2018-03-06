{-# LANGUAGE Arrows #-}

module ElevetorImpl where

import FRP.Yampa
import FRP.Yampa.Utilities

type Position = Double
type Distance = Double
type Velocity = Double

lower, upper :: Position
lower = 0
upper = 5

upRate, downRate :: Velocity
upRate = 1.0
downRate = 1.1

elevator :: SF (Event (), Event ()) Position
elevator = proc (lbp, rbp) -> do
    rec
        v <- dHold 0 -< stop `tag` 0 `lMerge` goUp `tag` upRate `lMerge` goDown `tag` (-downRate)
        y <- (lower +) ^<< integral -< v

        let atBottom = y <= lower
            atTop = y >= upper
            stopped = v == 0
            waitingBottom = atBottom && stopped
            waitingTop = atTop && stopped

        arriveBottom <- edge -< atBottom
        arriveTop <- edge -< atTop

        let setUp = lbp `tag` True
            setDown = rbp `tag` True

        resetUp <- iPre noEvent -< goUp `tag` False
        resetDown <- iPre noEvent -< goDown `tag` False

        upPending <- hold False -< setUp `lMerge` resetUp
        downPending <- hold False -< setDown `lMerge` resetDown

        let pending = upPending || downPending
            eitherBotton = lbp `lMerge` rbp
            goDown = arriveTop `gate` pending `lMerge` eitherBotton `gate` waitingTop
            goUp = arriveBottom `gate` pending `lMerge` eitherBotton `gate` waitingBottom
            stop = (arriveTop `lMerge` arriveBottom) `gate` not pending

    returnA -< y
