module Sample2 (run) where

import Graphics.UI.GLUT

run :: IO ()
run = do
    getArgsAndInitialize
    mode <- putStrLn "select PrimitiveMode" >> getLine
    runImpl mode
    where
        runImpl :: String -> IO ()
        runImpl mode = do
            window <- createWindow "Sample2"
            reshapeCallback $= Just reshape
            displayCallback $= display mode
            mainLoop

reshape :: ReshapeCallback
reshape size = do
    viewport $= (Position 0 0, size)
    postRedisplay Nothing

display :: String -> DisplayCallback
display mode = do
    clear [ColorBuffer]
    renderPrimitive' mode
    flush
    where
        points :: [(GLfloat, GLfloat, GLfloat)]
        points = [(sin (2 * pi * k / 12), cos (2 * pi * k / 12), 0) | k <- [1..12]]

        coordinate :: IO ()
        coordinate = mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) points

        choicePrimitiveMode :: String -> PrimitiveMode
        choicePrimitiveMode "Points"        = Points
        choicePrimitiveMode "Triangles"     = Triangles
        choicePrimitiveMode "TriangleStrip" = TriangleStrip
        choicePrimitiveMode "TriangleFan"   = TriangleFan
        choicePrimitiveMode "Lines"         = Lines
        choicePrimitiveMode "LineLoop"      = LineLoop
        choicePrimitiveMode "LineStrip"     = LineStrip
        choicePrimitiveMode "Quads"         = Quads
        choicePrimitiveMode "QuadStrip"     = QuadStrip
        choicePrimitiveMode "Polygon"       = Polygon
        choicePrimitiveMode "Patches"       = Patches
        choicePrimitiveMode _               = error "No Choice"

        renderPrimitive' :: String -> IO ()
        renderPrimitive' mode = flip renderPrimitive coordinate $ choicePrimitiveMode mode
