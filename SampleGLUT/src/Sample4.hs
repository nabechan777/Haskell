module Sample4 (run) where

import Graphics.UI.GLUT
import Control.Monad

run :: IO ()
run = do
    getArgsAndInitialize
    createWindow "Sample4"
    displayCallback $= display
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just keyboardMouse
    mainLoop

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    scale 0.7 0.7 (0.7 :: GLfloat)
    forM_ (points 7) $ \(x, y, z) ->
        preservingMatrix $ do
            color $ Color3 ((x+1)/2) ((y+1)/2) ((z+1)/2)
            translate $ Vector3 x y z
            cube 0.1
    flush
    where
        points :: Int -> [(GLfloat, GLfloat, GLfloat)]
        points n = [(sin (2*pi*k/n'), cos (2*pi*k/n'), 0) | k <- [1..n']]
            where
                n' = fromIntegral n

reshape :: ReshapeCallback
reshape size = do
    viewport $= (Position 0 0, size)
    postRedisplay Nothing

keyboardMouse :: KeyboardMouseCallback
keyboardMouse key state modifier position = return ()

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
