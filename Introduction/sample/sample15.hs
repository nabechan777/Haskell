{-
    コマンドライン引数サンプル
    ・getArgs :: IO [String]
    ・getProgName :: IO String
-}

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    print args
    name <- getProgName
    putStrLn name
