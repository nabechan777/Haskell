{-
    練習問題（Maybeモナド）
    ・異常検知した時点でNothingを返す。
    ・Nothing <- 関数が値を返すことができないことを明示する
    ・"<-", guard <- 処理の途中で脱出する。
    ・mapMaybe <- 成功と失敗の混じったリストから成功の部分だけ抽出する。
    ・<|> <- 何種類かの処理を試行する。
-}

module Practice12
    ( result_practice12
    ) where


import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Char

factbyMaybe :: Int -> Maybe Int
factbyMaybe 0 = return 1 :: Maybe Int
factbyMaybe n | n > 0 = (n *) <$> factbyMaybe (n - 1)
              | otherwise = Nothing

facts :: Int -> ([Maybe Int], [Int])
facts n = (map factbyMaybe [n, n - 1, n - 2], mapMaybe factbyMaybe [n, n - 1, n - 2])

fibByMaybe :: Int -> Maybe Int
fibByMaybe 0 = return 0 :: Maybe Int
fibByMaybe 1 = return 1 :: Maybe Int
fibByMaybe n | n > 1 = (+) <$> fibByMaybe (n - 1) <*> fibByMaybe (n - 2)
             | otherwise = Nothing

return' :: a -> Maybe a
return' x = Just x

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just x) f = f x

mapMaybe' :: (a -> Maybe b) -> [a] -> [b]
mapMaybe' _ [] = []
mapMaybe' f (x:xs) = case f x of
    Just y -> y : mapMaybe f xs
    Nothing -> mapMaybe f xs

mapMaybeByFoldr :: (a -> Maybe b) -> [a] -> [b]
mapMaybeByFoldr f = (`foldr` []) $ \x xs -> case f x of
    Just x' -> x':xs
    Nothing -> xs

numUpper :: Int -> Int -> String -> Maybe String
numUpper x y s = do
    guard $ length s == x + y
    guard $ length (filter isDigit $ take x s) == x
    guard $ length (filter isUpper $ drop x s) == y
    Just s

check :: String -> Maybe String
check s = do
    guard $ length s == 3
    do
        guard $ isDigit $ s !! 0
        guard $ isUpper $ s !! 1
        guard $ isLower $ s !! 2
        <|> do
        guard $ isUpper $ s !! 0
        guard $ isUpper $ s !! 1
        guard $ isLower $ s !! 2
    Just s

result_practice12 :: IO ()
result_practice12 = do
    putStrLn "description:\n\tExsample of \"Maybe Monad\"\n"
    putStr "factbyMaybe -1 = "
    print $ factbyMaybe (-1)
    putStr "factbyMaybe 5 = "
    print $ factbyMaybe 5
    putStr "fibByMaybe -1 = "
    print $ fibByMaybe $ -1
    putStr "fibByMaybe 5 = "
    print $ fibByMaybe 5
    putStr "bind = "
    print $ Just 1 `bind` \a -> Just $ a + 2
    putStr "facts 1 = "
    print $ facts 1
    putStr "mapMaybe factbyMaybe [1, 0, -1] = "
    print $ mapMaybe factbyMaybe [1, 0, -1]
    putStr "mapMaybeByFoldr factbyMaybe [1, 0, -1] = "
    print $ mapMaybeByFoldr factbyMaybe [1, 0, -1]
    putStr "numUpper 3 2 \"123AB\" = "
    print $ numUpper 3 2 "123AB"
    putStr "numUpper 2 3 \"123AB\" = "
    print $ numUpper 2 3 "123AB"
    putStr "check \"1\" = "
    print $ check "1"
    putStr "check \"1Ab\" = "
    print $ check "1Ab"
    putStr "check \"ABb\" = "
    print $ check "ABb"
