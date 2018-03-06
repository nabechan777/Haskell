{-
    練習問題（モジュール）
-}
module Practice03
    ( result_practice03
    ) where

import Data.Char

rot13ch' :: Char -> Char
rot13ch' ch
    | 'A' <= ch && ch <= 'M' || 'a' <= ch && ch <= 'm' =
        chr $ ord ch + 13
    | 'N' <= ch && ch <= 'Z' || 'n' <= ch && ch <= 'z' =
        chr $ ord ch - 13
    | otherwise =
        ch

rot13' :: String -> String
rot13' "" = ""
rot13' (x:xs) = rot13ch' x : rot13' xs

result_practice03 :: IO ()
result_practice03 = do
    let target = "Hello Haskell!!"
    let encripted = rot13' target
    putStrLn "description:\n\tDefine function to be encripted with \"Rot13\" method.\n"
    putStr "target = "
    print target
    putStr "rot13 target = "
    print encripted
    putStr "rot13 (rot13 target) = "
    print $ rot13' encripted

main = do
    let encription' = rot13' "Hello, world!!"
    print encription'
    print $ rot13' encription'
