module B where

import System.IO
import Text.Regex
import Control.Monad

main :: IO ()
main = do
    ihandle <- openFile "../DataSet/B.txt" ReadMode
    ohandle <- openFile "../Result/B.txt" WriteMode
    t <- fmap read $ hGetLine ihandle :: IO Int
    forM_ [1..t] $ \i -> do
        n <- fmap read (hGetLine ihandle)
        hPutStrLn ohandle ("Case #" ++ show i)
        results <- predicateFriend ihandle n
        putFriends ohandle results
    hClose ihandle
    hClose ohandle
    where
        putFriends :: Handle -> [String] -> IO ()
        putFriends handle [] = hPutStrLn handle "Not Friends"
        putFriends handle results = mapM_ (hPutStrLn handle) results

predicateFriend :: Handle -> Int -> IO [String]
predicateFriend handle n = do
    xs <- sequence $ replicate n (hGetLine handle)
    let results = map (\x -> x !! 0) $ filter (\x -> (x !! 1) == "Friend") $ map (\x -> splitRegex (mkRegex ":") x) xs
    return results
