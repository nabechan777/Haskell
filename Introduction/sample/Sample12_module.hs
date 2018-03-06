{-
    モジュールサンプル
-}

module Sample12_module where
    data Shape = Tri { base, height :: Double} | Rect { width, height :: Double }
    
    area :: Shape -> Double
    area (Tri b h) = b * h / 2
    area (Rect w h) = w * h
