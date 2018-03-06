{-
    IOアクション
    ・入出力などの副作用が伴う演算はIOアクションで表される。
    ・main関数はIO()型である。
    ・IO()型は意味のある値は返さないことを示す。

    do式
    ・3種類の式がかける
    ・アクション
    ・パターン <- アクション：IO a型であればa型の値が束縛される。
    ・let宣言
-}

main :: IO ()
main = do
    putStrLn "What's you are name ?"
    name <- getLine
    let greeting = "Hello " ++ name ++ "!!"
    putStrLn greeting
