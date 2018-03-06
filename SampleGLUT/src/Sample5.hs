module Sample5 where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad

run :: IO ()
run = do
    getArgsAndInitialize
    createWindow "Sample5"
    initialDisplayMode $= [DoubleBuffered]
    reshapeCallback $= Just reshape
    angle <- newIORef 0.0
    delta <- newIORef 1.0
    pos   <- newIORef (0,0)
    keyboardMouseCallback $= Just (keyboardMouse delta pos)
    idleCallback $= Just (idle angle delta)
    displayCallback $= display angle pos
    mainLoop

display :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> DisplayCallback
display angle pos = do
    clear [ColorBuffer]
    loadIdentity
    (x', y') <- get pos
    translate $ Vector3 x' y' 0
    preservingMatrix $ do
        a <- get angle
        rotate a $ Vector3 0 0 1
        scale 0.7 0.7 (0.7 :: GLfloat)
        forM_ (points 7) $ \(x, y, z) ->
            preservingMatrix $ do
                color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
                translate $ Vector3 x y z
                cube 0.1
    swapBuffers
    where
        points :: Int -> [(GLfloat, GLfloat, GLfloat)]
        points n = [(sin (2*pi*k/n'), cos (2*pi*k/n'), 0) | k <- [1..n']]
            where n' = fromIntegral n

idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
    d <- get delta
    angle $~! (+ d)
    postRedisplay Nothing

reshape :: ReshapeCallback
reshape size = do
    viewport $= (Position 0 0, size)
    postRedisplay Nothing

keyboardMouse :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse a p key Down _ _ = case key of
    (Char ' ')            -> a $~! negate
    (Char '+')            -> a $~! (* 2)
    (Char '-')            -> a $~! (/ 2)
    (SpecialKey KeyLeft ) -> p $~! \(x, y) -> (x-0.1, y)
    (SpecialKey KeyRight) -> p $~! \(x, y) -> (x+0.1, y)
    (SpecialKey KeyUp   ) -> p $~! \(x, y) -> (x, y+0.1)
    (SpecialKey KeyDown ) -> p $~! \(x, y) -> (x, y-0.1)
    _                     -> return ()
keyboardMouse _ _ _ _ _ _ = return ()
cube :: GLfloat -> IO ()
cube w = renderPrimitive Quads $ mapM_ vertex3f
    [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
    ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
    ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
    (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
    ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
    ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]
    where
        vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
        vertex3f (x, y, z) = vertex $ Vertex3 x y z
