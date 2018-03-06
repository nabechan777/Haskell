{-
    IOモナドサンプル
-}

main :: IO ()
main = do
    from <- getLine
    to <- getLine
    putStrLn("copy " ++ from ++ "to " ++ to)
    contents <- readFile from
    writeFile to contents
