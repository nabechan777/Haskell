module A where

import System.IO
import Control.Monad

main :: IO ()
main = do
    ihandle <- openFile "../DataSet/A.txt" ReadMode
    ohandle <- openFile "../Result/A.txt" WriteMode
    t <- fmap read $ hGetLine ihandle :: IO Int
    forM_ [1..t] $ \i -> do
        hPutStrLn ohandle ("Case #" ++ show i)
        result <- predicateLate ihandle
        hPutStrLn ohandle result
    hClose ihandle
    hClose ohandle

predicateLate :: Handle -> IO String
predicateLate handle = do
    cs <- fmap words $ hGetLine handle
    let toInt :: Int -> [String] -> Int
        toInt n = read . (!! n)
        s = toInt 0 cs
        o = toInt 1 cs
        c = toInt 2 cs
    if s > o + c
        then return $ show (o+c)
        else return "NG"
