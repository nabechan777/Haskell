{-
    練習問題（リストモナド）
    ・IOモナドと違い、モナド内の値をバインドを使わずに参照することができる。
-}

module Practice11
    ( result_practice11
    ) where

import Control.Monad

sampleListMonad :: IO ()
sampleListMonad = do
    putStr "print [1] = "
    print [1]
    putStr "return 1 :: [Int] = "
    print (return 1 :: [Int])
    putStr "return 1 :: [] Int = "
    print (return 1 :: [] Int)
    putStr "1 : 2 : return 3 :: [Int] = "
    print (1 : 2 : return 3 :: [Int])
    putStr "join [[1,2],[3]] = "
    print $ join [[1,2],[3]]
    putStr "combination [1..3] \"abc\" = "
    print $ join $ join $
        forM [1..3] $ \x ->
            forM "abc" $ \y ->
                return (x, y)


return' :: a -> [] a
return' x = [x]

bind :: [] a -> (a -> [] b) -> [] b
bind xs f = foldr ((++) . f) [] xs

rewriteComprehensiveNotation :: [] (Int, Int)
rewriteComprehensiveNotation = do
    x <- [1..5]
    y <- [1..5]
    if (x + y == 6) then return (x, y) else []

comprehensiveNotationByBindAndReturn :: [(Int, Int)]
comprehensiveNotationByBindAndReturn = do
    [1..5] `bind` \x -> [1..5] `bind` \y -> if x + y == 6 then return' (x, y) else []

result_practice11 :: IO ()
result_practice11 = do
    putStrLn "description;\n\tExsample of \"List Monad\"\n"
    putStrLn "<<<<<<<<<< sampleListMonad >>>>>>>>>>"
    sampleListMonad
    putStrLn ""
    putStr "print $ [1..3] `bind` \\x -> \"abc\" `bind` \\y -> return' (x, y) = "
    print $ [1..3] `bind` \x -> "abc" `bind` \y -> return' (x, y)
    putStr "[(x, y) | x <- [1..5], y <- [1..5], x + y == 6] = "
    print $ [(x, y) | x <- [1..5], y <- [1..5], x + y == 6]
    putStr "rewriteComprehensiveNotation [1..5] [1..5] = "
    print $ rewriteComprehensiveNotation
    putStr "comprehensiveNotationByBindAndReturn = "
    print $ comprehensiveNotationByBindAndReturn
