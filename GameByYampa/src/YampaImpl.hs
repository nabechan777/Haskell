{-# LANGUAGE Arrows #-}

module YampaImpl where

import FRP.Yampa
import Control.Concurrent
import FRP.Yampa.Vector3
import FRP.Yampa.Utilities
import Unsafe.Coerce
import Data.IORef

import Graphics.UI.GLUT hiding (Level, Vector3(..), normalize)
import qualified Graphics.UI.GLUT as G(Vector3(..))

type PosX = Double
type PosY = Double
type VelX = Double
type VelY = Double
type Pos = (PosX, PosY)
type Vel = (VelX, VelY)
type R = GLdouble

fallingBall :: Pos -> Vel -> SF () (Pos, Vel)
fallingBall pos0 vel0 = proc input -> do
    vel <- integral >>^ (vel0 ^+^) -< (0.0, -9.81)
    pos <- integral >>^ (pos0 ^+^) -< vel
    returnA -< (pos, vel)
-- fallingBall y0 = (constant (-9.81) >>> integral) >>> ((integral >>^ (+ y0)) &&& identity)

throwingStone :: Pos -> Vel -> SF () (Pos, Vel)
throwingStone pos0 vel0 = switch (bb pos0 vel0) (\(pos, vel) -> if abs (snd vel) <= 1 then constant ((fst pos, 0), (0, 0)) else throwingStone pos (fst vel, ((- (snd vel)) * 0.6)))
    where
        bb pos0' vel0' = proc input -> do
            (pos, vel) <- fallingBall pos0' vel0' -< input
            event <- edge -< snd pos <= 0
            returnA -< ((pos, vel), event `tag` (pos, vel))

bird :: Pos -> Vel -> SF () (Pos, Vel)
bird pos0 vel0 = proc input -> do
    vel <- integral >>^ (vel0 ^+^) -< (0, 0)
    pos <- integral >>^ (pos0 ^+^) -< vel
    returnA -< (pos, vel)

initGL :: IO ()
initGL = do
    getArgsAndInitialize
    createWindow "Bounce"
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    depthFunc $= Just Less
    clearColor $= Color4 0 0 0 0
    light (Light 0) $= Enabled
    lighting $= Enabled
    lightModelAmbient $= Color4 0.5 0.5 0.5 1
    diffuse (Light 0) $= Color4 1 1 1 1
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
    displayCallback $= return ()
    reshapeCallback $= Just resizeScene
    return ()

resizeScene :: Size -> IO ()
resizeScene (Size w 0) = resizeScene (Size w 1)
resizeScene s@(Size width height) = do
    viewport $= (Position 0 0, s)
    matrixMode $= Projection
    loadIdentity
    perspective 45 (w2/h2) 1 1000
    matrixMode $= Modelview 0
    where
        w2 = half width
        h2 = half height
        half z = realToFrac z/2

clearAndRender :: ((Pos, Vel), (Pos, Vel), (Pos, Vel)) -> IO ()
clearAndRender ((pos1, vel1), (pos2, vel2), (pos3, vel3)) = do
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    renderPlayer $ vector3 (unsafeCoerce $ fst pos1) (unsafeCoerce $ snd pos1) (-30)
    renderPlayer $ vector3 (unsafeCoerce $ fst pos2) (unsafeCoerce $ snd pos2) (-30)
    renderPlayer $ vector3 (unsafeCoerce $ fst pos3) (unsafeCoerce $ snd pos3) (-30)
    flush
    where
        size2 :: R
        size2 = (fromInteger 6) / 2
        green = Color4 0.8 1.0 0.7 0.9 :: Color4 R
        greenG = Color4 0.8 1.0 0.7 1.0 :: Color4 R
        red = Color4 1.0 0.7 0.8 1.0 :: Color4 R
        renderShapeAt s p = preservingMatrix $ do
            translate $ G.Vector3 (0.5 - size2 + vector3X p) (0.5 - size2 + vector3Y p) (0.5 - size2 + vector3Z p)
            renderObject Solid s
        renderObstacle = (color green >>) . (renderShapeAt $ Cube 1)
        renderPlayer = (color red >>) . (renderShapeAt $ Sphere' 0.5 20 20)
        renderGoal = (color greenG >>) . (renderShapeAt $ Sphere' 0.5 20 20)

idle :: IORef Int -> ReactHandle (Event ()) (IO ()) -> IO ()
idle oldTime rh = do
    newTime' <- get elapsedTime
    oldTime' <- get oldTime
    let dt = (fromIntegral $ newTime' - oldTime') / 1000
    react rh (dt, Nothing)
    writeIORef oldTime newTime'
    return ()

simulate :: SF (Event ()) (IO ())
simulate = discardInputs >>> update >>> draw

discardInputs :: SF (Event ()) ()
discardInputs = constant ()

update :: SF () ((Pos, Vel), (Pos, Vel), (Pos, Vel))
update = proc () -> do
    (pos1, vel1) <- throwingStone ((-8.0), 0) (1, 15) -< ()
    (pos2, vel2) <- throwingStone ((-8.0), 0) (4, 17) -< ()
    (pos3, vel3) <- bird ((-8), 10) (4, 0) -< ()
    returnA -< ((pos1, vel1), (pos2, vel2), (pos3, vel3))

draw :: SF ((Pos, Vel), (Pos, Vel), (Pos, Vel)) (IO ())
draw = arr clearAndRender

-- runSF = (bouncingBall 10.0 0.0) >>^ (\(pos, vel) -> putStrLn ("pos: " ++ show pos ++ ", vel: " ++ show vel) >> draw pos)
run :: IO ()
run = do
    oldTime <- newIORef (0 :: Int)
    rh <- reactInit (initGL >> return NoEvent) (\ _ _ b -> b >> return False) simulate
    displayCallback $= return ()
    idleCallback $= Just (idle oldTime rh)
    newTime <- get elapsedTime
    writeIORef oldTime newTime
    mainLoop
-- run = reactimate initialization input output sf
--     where
--         -- 初期状態
--         initialization :: IO ()
--         initialization = initGL
--         -- 入力（時間とイベントを与える）<- 入力が偽の時、前回サンプルを採取した後から経過した時間と新しいa型のサンプルとのタプルを返す
--         -- aがNotingである場合、前回のサンプルが再び使用する。
--         input :: Bool -> IO (DTime, Maybe a)
--         input = \_ -> threadDelay 1000000 >> return (0.1, Nothing)
--         -- 出力（シグナル関数の出力を得る）<- 入力が真の時、b型でシグナル関数を出力する。
--         -- 処理を終了したい場合は真を返し、継続する場合は偽を返す。
--         output :: Bool -> (Pos, Vel) -> IO Bool
--         output = \_ (pos, vel) -> putStrLn ("pos: " ++ show pos ++ ", vel: " ++ show vel) >> draw pos >> return False
--         -- シグナル関数 <- 高さと速度のサンプルを採取する。
--         sf = fallingBall 10.0
