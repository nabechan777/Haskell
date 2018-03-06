module A where

main :: IO ()
main = do
    a <- fmap read getLine
    (b:c:_) <- fmap ((map read) . words) getLine
    s <- getLine
    let result = a + b + c :: Int
    putStrLn $ show result ++ " " ++ s
