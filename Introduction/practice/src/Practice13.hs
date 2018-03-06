{-
    練習問題（StateSystemモナド）
    ・STモナド
        <- 破壊的代入に特化したモナド。
        <- 状態を渡すと更新後の状態と値がアンボックスタプルとして返ってくる。
        <- runST関数でモナドの枠組みを外して、値を取りだことができる。

    ・Stateモナド
        <- 仕様がオープンになっているSTモナド。
        <- 状態を渡すと更新後の状態と値が普通のタプルで返ってくる。
        <- runState関数で値を取り出すことができる。
        <- runState :: State s a -> s -> (a, s)
        <- しかしrunSTと違って、明示的に状態を渡す必要がある。
        <- また返値は（値, 状態）の形で返ってくる。
        <- evalState :: State s a -> s -> a（値だけを返す）
        <- execState :: State s a -> s -> s（状態だけを返す）
        <- IOアクションに対して、Stateアクションがある。
        <- get :: State s s（値を参照する）
        <- put :: s -> State s (値を書き換える)
        <- modify :: (s -> s) -> State s (値を関数を通して書き換える)
        <- state :: (a -> (a, s)) -> State s a（Stateモナドの生成）

    ・Readerモナド
        <- 読み取り専用のStateモナド。（明示的な状態の変更を禁止する。）
        <- ask :: Reader r r（状態を読み取る）
        <- runReader :: Reader r a -> r -> a（値を返す。）
        <- reader :: (r -> a) -> Reader r a（Readerモナドを生成する。）

    ・Writerモナド
        <- 追記専用のStateモナド
        <- tell :: w -> Writer w ()（状態を追加する）
        <- runWriter :: Writer w a -> (a, w)（値の取り出し）
        <- writer :: MonadWriter w m => (a, w) -> m a（Writerモナドの生成）

    ・関数モナド
        <- 引数を状態として受け取る。
        <- Readerモナドに似ている。


-}
{-# LANGUAGE UnboxedTuples #-}

module Practice13
    ( result_practice13
    ) where

import GHC.ST
import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.STRef
import Data.Array.ST
import Data.Functor.Identity

sum' :: [Int] -> Int
sum' xs = runST $ do
    v <- newSTRef 0
    forM_ xs $ \i ->
        modifySTRef v (+ i)
    readSTRef v

preprocessBrainF_ck :: String -> [Int]
preprocessBrainF_ck s = runST $ do
        let bfl = length s - 1
        jmp <- newArray (0, bfl) 0 :: ST s (STUArray s Int Int)
        loops <- newSTRef []
        forM_ [0..bfl] $ \i -> do
            case s !! i of
                '[' -> modifySTRef loops (i:)
                ']' -> do
                    start <- do
                        (h:t) <- readSTRef loops
                        writeSTRef loops t
                        return h
                    writeArray jmp start i
                    writeArray jmp i start
                _ -> return ()
        getElems jmp

unST :: GHC.ST.ST s a -> GHC.ST.STRep s a
unST (ST f) = f

returnOfST :: a -> ST s a
returnOfST x = ST $ \s -> (# s, x #)

bindOfST :: ST s a -> (a -> ST s b) -> ST s b
bindOfST x f = ST $ \s ->
    let (# s1, r1 #) = unST x s
        (# s2, r2 #) = unST (f r1) s1
    in (# s2, r2 #)

exsampleStateAction :: StateT Integer Identity Integer
exsampleStateAction = do
    a <- get
    put $ a + 1
    modify (* 2)
    return a

sumByStateAction :: (Foldable t, Num s) => t s -> s
sumByStateAction xs = (`execState` 0) $ do
    forM_ xs $ \i ->
        modify (+ i)

bindForState :: MonadState s m => State s t -> (t -> State s a) -> m a
bindForState x f = state $ \s ->
    let (r1, s1) = runState x s
        (r2, s2) = runState (f r1) s1
    in (r2, s2)

returnForState :: MonadState s m => a -> m a
returnForState x = state $ \s -> (x, s)


getForState :: State s s
getForState = state $ \s -> (s, s)
-- putForState :: s -> MonadState s
putForState :: MonadState s m => s -> m ()
putForState x = state $ \_ -> ((), x)

fibForState :: Num a => Int -> a
fibForState x = (`evalState` (0, 1)) $
    (replicateM_ (x - 1) $
        getForState `bindForState` \(a, b) -> putForState (b, a + b)) `bindForState` \_ ->
    getForState `bindForState` \v -> returnForState $ snd v

bindForReader :: MonadReader r m => Reader r t -> (t -> Reader r a) -> m a
bindForReader x f = reader $ \r ->
    runReader (f (runReader x r)) r

returnForReader :: MonadReader r m => a -> m a
returnForReader x = reader $ \_ -> x

askForReader :: Reader r r
askForReader = reader $ \r -> r

localForReader :: MonadReader t m => (t -> r) -> (r -> a) -> m a
localForReader f m = reader $ \r -> reader m (f r)

testOfReaderByBind :: Num t => t -> (t, t, t)
testOfReaderByBind x = (`runReader` x) $
    askForReader `bindForReader` \a ->
        (localForReader (+ 1) $ askForReader `bindForReader` \b' ->
            returnForReader b') `bindForReader` \b ->
        askForReader `bindForReader` \c ->
        returnForReader (a, b, c)

testOfReaderByDo :: Num t => t -> (t, t, t)
testOfReaderByDo x = (`runReader` x ) $ do
    a <- ask
    b <- let b' = ask in local (+ 1) b'
    c <- ask
    return (a, b, c)

-- bindForWriter m f = writer $
--     let (r1, w1) = runWriter m
--         (r2, w2) = runWriter (f r1)
--     in (r2, w1 ++ w2)
-- returnForWriter x = writer (x, [])
-- tellForWriter x = writer ((), x)

testWriterByBind :: String
testWriterByBind = execWriter $
    tell "Hello" >>= \_ ->
    tell ", " >>= \_ ->
    tell "World" >>= \_ ->
    tell "!!" >>= \_ ->
    return ()

testWriterByDo :: String
testWriterByDo = execWriter $ do
    tell "Hello"
    tell ", "
    tell "World"
    tell "!!"
    return ()

testFunctionMonad :: Num a => a -> (a, a)
testFunctionMonad = do
    a <- (+ 2)
    b <- (* 2)
    return (a, b)

testFunction :: Num a => a -> (a, a)
testFunction x = (a, b)
    where
        a = (+ 2) x
        b = (* 2) x

sampleState :: IO ()
sampleState = do
    let a = return 1 :: State s Int
    putStr "runState a () = "
    print $ runState a ()
    putStr "evalState a () = "
    print $ evalState a ()
    putStr "execState a () = "
    print $ execState a ()
    putStr "runState exsampleStateAction 5 = "
    print $ runState exsampleStateAction 5
    putStr "sumByStateAction [1..100] = "
    print $ sumByStateAction [1..100]
    putStr "fibForState 10 = "
    print $ fibForState 10


result_practice13 :: IO ()
result_practice13 = do
    putStrLn "description:\n\tExsample of \"StateSystem Monad\".\n"
    putStr "sum' [1..100] = "
    print $ sum' [1..100]
    putStr "preprocessBrainF_ck \"+++++++++[>++++++++<-]>.\" = "
    print $ preprocessBrainF_ck "+++++++++[>++++++++<-]>."
    putStr "bind and return for ST Monad = "
    print $ runST $ returnOfST 1 `bindOfST` newSTRef `bindOfST` \a -> modifySTRef a (+ 1) `bindOfST` \_ -> readSTRef a `bindOfST` returnOfST
    sampleState
    putStr "testOfReaderByBind 1 = "
    print $ testOfReaderByBind 1
    putStr "testOfReaderByDo 1 = "
    print $ testOfReaderByDo 1
    putStr "testWriterByBind = "
    print $ testWriterByBind
    putStr "testOfReaderByDo = "
    print $ testWriterByDo
    putStr "testFunctionMonad 5 = "
    print $ testFunctionMonad 5
    putStr "testFunction 5 = "
    print $ testFunction 5
