module Sample3 (run) where

import Graphics.UI.GLUT

run :: IO ()
run = do
    (progname, args) <- getArgsAndInitialize
    window <- createWindow "Sample3"
    displayCallback $= display
    reshapeCallback $= Just reshape
    mainLoop

reshape :: ReshapeCallback
reshape size = do
    viewport $= (Position 0 0, size)
    postRedisplay Nothing

display :: DisplayCallback
display = do
    clear [ColorBuffer]
    renderPrimitive Quads $ do
        color3f (1, 0, 0)
        vertex3f (0, 0, 0)
        vertex3f (0, 0.2, 0)
        vertex3f (0.2, 0.2, 0)
        vertex3f (0.2, 0, 0)

        color3f (0, 1, 0)
        vertex3f (0, 0, 0)
        vertex3f (0, -0.2, 0)
        vertex3f (0.2, -0.2, 0)
        vertex3f (0.2, 0, 0)

        color3f (0, 0, 1)
        vertex3f (0, 0, 0)
        vertex3f (0, -0.2, 0)
        vertex3f (-0.2, -0.2, 0)
        vertex3f (-0.2, 0, 0)

        color3f (1, 0, 1)
        vertex3f (0, 0, 0)
        vertex3f (0, 0.2, 0)
        vertex3f (-0.2, 0.2, 0)
        vertex3f (-0.2, 0, 0)
    flush
    where
        color3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
        color3f (r, g, b) = color $ Color3 r g b

        vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
        vertex3f (x, y, z) = vertex $ Vertex3 x y z
