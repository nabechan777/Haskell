{-
    ファイル入出力サンプル
    ・readFile :: FilePath -> IO String ＜ファイルの読み込み＞
    ・writeFile :: FilePath -> String -> IO () ＜ファイルへの書き込み＞
    ・appendFile :: FilePath -> String -> IO () ＜既存のファイルへの追記＞
    ・openFile :: FilePath -> IOMode -> IO Handle
    ・hGetContents :: Handle -> IO String
    ・hClose :: Handle -> IO ()
-}

import System.IO

main :: IO ()
main = do
    handle <- openFile "sample14.hs" ReadMode
    text <- hGetContents handle
    putStr text
    hClose handle
