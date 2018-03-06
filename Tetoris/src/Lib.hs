module Lib
    ( someFunc
    ) where

import Graphics.UI.GLUT

someFunc :: IO ()
someFunc = do
    getArgsAndInitialize
    createWindow "Tetoris"
    windowSize $= Size 500 600
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just keyboardMouse
    idleCallback $= Just idle
    displayCallback $= display
    mainLoop

reshape :: ReshapeCallback
reshape size = do
    viewport $= (Position 0 0, size)
    postRedisplay Nothing

keyboardMouse :: KeyboardMouseCallback
keyboardMouse key state modifier position = return ()

idle :: IdleCallback
idle = return ()

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    renderPrimitive Polygon $ mapM_ vertex3f $ points 0.1
    flush
    where
        points :: GLfloat -> [(GLfloat, GLfloat, GLfloat)]
        points w = [(w, w, 0), (-w, w, 0), (-w, -w, 0), (w, -w, 0)]

        vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
        vertex3f (x, y, z) = vertex $ Vertex3 x y z
