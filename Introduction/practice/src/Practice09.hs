{-
    練習問題（アクションとラムダ）
    ・doブロックはラムダ式をバインドで繋ぐことで書き換えることができる
    ・replicateM（任意の回数、アクションから値を取り出す。取り出した値のリストを返す。）
    ・replicateM_ （値を返さない。それ以外は同じ）
    ・forM（繰り返し処理をする。処理の結果の値のリストを返す。）
    ・forM_（値のリストを返さない）
-}

module Practice09
    ( result_practice09
    ) where

import System.Random
import Data.IORef
import Data.Array.IO
import Control.Applicative
import Control.Monad

helloWorldWithDo :: IO ()
helloWorldWithDo = do
    print "Hello"
    print "World"

helloWorldWithOutDo :: IO ()
helloWorldWithOutDo =
    print "Hello" >>= \_ -> print "World"

variableBindWithDo :: IO ()
variableBindWithDo = do
    a <- return "Haskell"
    print a

variableBindWithOutDo :: IO ()
variableBindWithOutDo =
    return "Haskell" >>= \a -> print a

shulleWithOutDo :: [Int] -> IO [Int]
shulleWithOutDo [] = return []
shulleWithOutDo xs =
    (getStdRandom $ randomR (0, length xs - 1) :: IO Int) >>= \n
        -> (shulleWithOutDo $ take n xs ++ drop (n + 1) xs) >>= \xs'
            -> return $ (xs !! n) : xs'


incrementWithIORef :: IO Int
incrementWithIORef = do
    a <- newIORef =<< (getStdRandom $ randomR (0, 9) :: IO Int)
    a' <- readIORef a
    writeIORef a (a' + 1)
    readIORef a >>= return

incrementWithIORefByModifiyIORef :: IO Int
incrementWithIORefByModifiyIORef = do
    a <- newIORef =<< (getStdRandom $ randomR (0, 9) :: IO Int)
    modifyIORef a (+ 1)
    readIORef a >>= return

modifyArrayRef :: (IOUArray Int Int) -> Int -> (Int -> Int) -> IO ()
modifyArrayRef array i f = f <$> readArray array i >>= writeArray array i

dice :: IO Int
dice = getStdRandom $ randomR (0, 6) :: IO Int

replicateActions :: IO ()
replicateActions = do
    putStr "replicate 5 1 = "
    print $ replicate 5 1
    putStr "replicateM 5 dice = "
    replicateM 5 dice >>= print
    putStr "replicateM_ 5 dice = "
    replicateM_ 5 dice >>= print
    putStrLn ""

forActions :: IO ()
forActions = do
    putStrLn "<<<<<<<<<< forM >>>>>>>>>>"
    a <- forM [1..5] $ \i -> do
        print i
        return i
    print a
    putStrLn ""
    putStrLn "<<<<<<<<<< forM_ >>>>>>>>>>"
    a <- forM_ [1..5] $ \i -> do
        print i
        return i
    print a
    putStrLn ""

exsampleWhen :: IO ()
exsampleWhen = do
    a <- dice
    print a
    when (a /= 3) exsampleWhen

exsampleUnless :: IO ()
exsampleUnless = do
    a <- dice
    print a
    unless (a == 3) exsampleUnless

whenAndUnless :: IO ()
whenAndUnless = do
    putStrLn "<<<<<<<<<< exsampleWhen >>>>>>>>>>"
    exsampleWhen
    putStrLn "<<<<<<<<<< exsampleUnless >>>>>>>>>>"
    exsampleUnless
    putStrLn ""

replicateM' :: Monad m => Int -> m a -> m [a]
replicateM' 0 _ = return []
replicateM' i a | i > 0 = (:) <$> a <*> replicateM' (i - 1) a

replicateM_' :: Monad m => Int -> m a -> m ()
replicateM_' 0 _ = return ()
replicateM_' i a | i > 0 = a >> replicateM_' (i - 1) a

forM' :: Monad m => [a] -> (a -> m b) -> m [b]
forM' [] _ = return []
forM' (x:xs) f = (:) <$> f x <*> forM' xs f

forM_' :: Monad m => [a] -> (a -> m b) -> m ()
forM_' [] _ = return ()
forM_' (x:xs) f = f x >> forM_' xs f

when' :: Monad m => Bool -> m () -> m ()
when' b f | b == True = f >> return ()
          | otherwise = return ()

unless' :: Monad m => Bool -> m () -> m ()
unless' b f | b == False = f >> return ()
            | otherwise = return ()

destributionOfDecimalNumbers :: IO ()
destributionOfDecimalNumbers = do
    r <- replicateM 100 $ do
        rands <- replicateM 12 (getStdRandom $ randomR (0.0, 1.0) :: IO Double)
        return $ truncate $ (sum rands + 0.5) - 6
    forM_ [-3..3] $ \i -> do
        let c = length $ filter (== i) r
            i' = show i
            n = replicate (2 - length i') ' ' ++ i'
        putStrLn $ n ++ ": " ++ replicate c '*'

result_practice09 :: IO ()
result_practice09 = do
    putStrLn "<<<<<<<<<< helloWorldWithDo >>>>>>>>>>"
    helloWorldWithDo
    putStrLn ""
    putStrLn "<<<<<<<<<< helloWorldWithOutDo >>>>>>>>>>"
    helloWorldWithOutDo
    putStrLn ""
    putStrLn "<<<<<<<<<< variableBindWithDo >>>>>>>>>"
    variableBindWithDo
    putStrLn ""
    putStrLn "<<<<<<<<<< variableBindWithOutDo >>>>>>>>>>"
    variableBindWithOutDo
    putStrLn ""
    putStr "shulleWithOutDo [1..9] = "
    shulleWithOutDo [1..9] >>= print
    putStr "incrementWithIORef = "
    incrementWithIORef >>= print
    putStr "incrementWithIORefByModifiyIORef = "
    incrementWithIORefByModifiyIORef >>= print
    putStrLn ""
    putStrLn "<<<<<<<<<< modifyArrayRef >>>>>>>>>>"
    a <- newArray (0, 2) 0 :: IO (IOUArray Int Int)
    putStr "array = "
    getElems a >>= print
    modifyArrayRef a 1 (+ 5)
    putStr "array = "
    getElems a >>= print
    putStrLn ""
    replicateActions
    forActions
    whenAndUnless
    putStr "replicateM' 5 dice = "
    replicateM' 5 dice >>= print
    putStr "replicateM_' 5 dice = "
    replicateM_' 5 dice >>= print
    putStrLn "forM' [0..5] f = "
    let blockForM :: Int -> IO Int
        blockForM = \ i -> (when' (i == 3) $ print i) >> return i
    print =<< forM' [0..5] blockForM
    putStrLn "forM_' [0..5] f = "
    let blockForM_ :: Int -> IO Int
        blockForM_ = \ i -> (unless' (i /= 3) $ print i) >> return i
    print =<< forM_' [0..5] blockForM_
    putStrLn ""
    putStrLn "<<<<<<<<<< destributionOfDecimalNumbers >>>>>>>>>>"
    destributionOfDecimalNumbers
