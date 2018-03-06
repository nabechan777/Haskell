module Sample1 (run) where

import Graphics.UI.GLUT

run :: IO ()
run = do
    (progname, args) <- getArgsAndInitialize
    window <- createWindow "Hello OpenGL"
    reshapeCallback $= Just reshape
    displayCallback $= display
    mainLoop

reshape :: ReshapeCallback
reshape size = do
    viewport $= (Position 0 0, size)
    postRedisplay Nothing

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    flush
