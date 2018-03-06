{-----------------------------------------------------------------------------
    reactive-banana-wx

    Example: A simple animation.
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-}
    -- allows pattern signatures like
    -- do
    --     (b :: Behavior Int) <- stepper 0 ...
{-# LANGUAGE RecursiveDo #-}
    -- allows recursive do notation
    -- mdo
    --     ...

module Animation (run) where

import Graphics.UI.WX hiding (Event, Vector)
import Reactive.Banana
import Reactive.Banana.WX

-- import Paths (getDataFile)

{-----------------------------------------------------------------------------
    Constants
------------------------------------------------------------------------------}
height, width :: Int
height   = 400
width    = 400

dt :: Double
dt = 20 * ms where ms = 1e-3

sprite :: Bitmap ()
-- sprite = bitmap $ getDataFile "banana.png"
sprite = bitmap ".stack-work/install/x86_64-osx/lts-9.21/8.0.2/share/x86_64-osx-ghc-8.0.2/reactive-banana-wx-1.1.1.0/banana.png"

bitmapWidth, bitmapHeight :: Int
bitmapWidth  = 128
bitmapHeight = 128

{-----------------------------------------------------------------------------
    Run
------------------------------------------------------------------------------}
run :: IO ()
run = start $ do
    ff <- frame [ text       := "It's functional programming time"
                , bgcolor    := white
                , resizeable := False ]

    t  <- timer ff [ interval := ceiling (dt * 1e3) ]
    pp <- panel ff [ ]
    set ff [ layout  := minsize (sz width height) $ widget pp ]

    -- event network
    let networkDescription :: MomentIO ()
        networkDescription = mdo
            etick  <- event0 t command  -- frame timer
            emouse <- event1 pp mouse   -- mouse events

            -- mouse pointer position
            (bmouse :: Behavior Vector) <-
                fmap fromPoint <$> stepper (point 0 0)
                    (filterJust $ justMotion <$> emouse)

            let
                -- sprite velocity
                bvelocity :: Behavior Vector
                bvelocity =
                    (\pos mouse -> speedup $ mouse `vecSub` pos `vecSub` vec 0 45)
                    <$> bposition <*> bmouse
                    where
                    speedup v = v `vecScale` (vecLengthDouble v / 20)

            -- sprite position
            (bposition :: Behavior Vector)
                <- accumB (vec 0 0) $
                    (\v pos -> clipToFrame $ (v `vecScale` dt) `vecAdd` pos)
                    <$> bvelocity <@ etick

            let
                clipToFrame v = vec
                        (clip 0 x (fromIntegral $ width  - bitmapWidth ))
                        (clip 0 y (fromIntegral $ height - bitmapHeight))
                    where
                    x = vecX v; y = vecY v
                    clip a x b = max a (min x b)

                drawSprite :: Point -> DC a -> b -> IO ()
                drawSprite pos dc _view = drawBitmap dc sprite pos True []

            -- animate the sprite
            sink pp [on paint :== drawSprite . toPoint <$> bposition]
            reactimate $ repaint pp <$ etick

    network <- compile networkDescription
    actuate network

{-----------------------------------------------------------------------------
    2D Geometry
------------------------------------------------------------------------------}
type Vector = Vector2 Double

fromPoint :: Point -> Vector
fromPoint pt = vector (fromIntegral (pointX pt)) (fromIntegral (pointY pt))

toPoint :: Vector -> Point
toPoint v = point (ceiling (vecX v)) (ceiling (vecY v))

{-----------------------------------------------------------------------------
    wx stuff
------------------------------------------------------------------------------}
justMotion :: EventMouse -> Maybe Point
justMotion (MouseMotion pt _) = Just pt
justMotion _                  = Nothing
