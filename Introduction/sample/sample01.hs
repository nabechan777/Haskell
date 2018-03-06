{-
    複数の式の記述はdo{ ... }をつかう。各式はセミコロンで区切る。
    他にもwhere, let, ofでも{ ... }が現れる。
    1行で記述する際は{}を省略することができる。
-}
main = do { putStrLn "Hello"; putStrLn "Haskell"; }
