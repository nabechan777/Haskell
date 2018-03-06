{-
    練習問題（IOモナド）
    State# RealWorld <- IOモナド内で状態を示すID

-}

{-# LANGUAGE UnboxedTuples, MagicHash #-}

module Practice10
    ( result_practice10
    ) where

import System.Random
import GHC.Base

-- s -> s1 -> s2 -> s3の経路で状態が変化する。
sampleUnIO :: State# RealWorld -> (# State# RealWorld, () #)
sampleUnIO s = (# s3, r #)
    where
        (# s1, _ #) = unIO (print "Hello") s -- 状態s1は（print "Hello"）を実行する
        (# s2, _ #) = unIO (print "World") s1
        (# s3, r #) = unIO (print "!!") s2

-- 生の関数を定義したのちIOモナドに入れたshuffle関数
shuffle :: [Int] -> IO [Int]
shuffle [] = IO $ \s -> (# s, [] #)
shuffle xs = IO $ \s ->
    let
        (# s1, n #) = unIO (getStdRandom $ randomR (0, length xs - 1) :: IO Int) s
        (# s2, xs' #) = unIO (shuffle $ take n xs ++ drop (n + 1) xs) s1
    in (# s2, (xs !! n) : xs' #)

-- 生の関数を定義
shuffleNaked :: [Int] -> State# RealWorld -> (# State# RealWorld, [Int] #)
shuffleNaked [] = \s -> (# s, [] #)
shuffleNaked xs = \s ->
    let
        (# s1, n #) = unIO (getStdRandom $ randomR (0, length xs - 1) :: IO Int) s
        (# s2, xs' #) = (shuffleNaked (take n xs ++ drop (n + 1) xs)) s1
    in (# s2, (xs !! n) : xs' #)

testShuffle :: State# RealWorld -> (# State# RealWorld, () #)
testShuffle = \s ->
    let
        (# s1, xs #) = unIO (shuffle [1..9]) s
        (# s2, r #) = unIO (print xs) s1
    in (# s2, r #)

-- >>=の再実装
bind :: IO a -> (a -> IO b) -> IO b
bind x f = IO $ \s ->
    let (# s1, r1 #) = unIO x s
        (# s2, r2 #) = unIO (f r1) s1
    in (# s2, r2 #)

-- returnの再実装
return' :: a -> IO a
return' f = IO $ \s -> (# s, f #)

result_practice10 :: IO ()
result_practice10 = do
    putStrLn "description:\n\tStructure of \"IO Monad\"\n"
    putStrLn "<<<<<<<<<< sampleUnIO >>>>>>>>>>"
    IO sampleUnIO
    putStrLn ""
    putStr "shuffle [1..9] = "
    shuffle [1..9] >>= print
    putStr "IO shuffleNaked [1..9] = "
    (IO $ shuffleNaked [1..9]) >>= print
    putStr "return' \"Haskell\" `bind` print = "
    return' "Haskell" `bind` print
