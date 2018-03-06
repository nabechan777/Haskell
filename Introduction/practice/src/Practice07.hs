{-
    練習問題（アクション）
    アクション：副作用の伴う操作
-}

module Practice07
    ( result_practice07
    ) where

import System.Random
import Control.Applicative
import Debug.Trace
import Data.IORef
import Data.Array.IO
import Data.Char
import Data.Word
import System.IO.Unsafe

randomAlpha :: IO Char
randomAlpha = getStdRandom $ randomR ('a', 'z')

untilLowerCaseZ :: IO ()
untilLowerCaseZ = do
    alpha <- randomAlpha
    putStr [alpha, ',', ' ']
    case alpha of
        'z' -> putStrLn "End"
        otherwise -> untilLowerCaseZ

factByAction :: Int -> IO Int
factByAction 0 = return 1
factByAction n | n > 0 = do
    n' <- factByAction (n - 1)
    return $ n * n'

shuffle :: [Int] -> IO [Int]
shuffle [] = return []
shuffle xs = do
    n <- getStdRandom $ randomR (0, length xs - 1)::IO Int
    xs' <- shuffle $ take n xs ++ drop (n + 1) xs
    return $ (xs !! n) : xs'

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:zs)
    | x < y = isSorted (y:zs)
    | otherwise = False

bogoSort :: [Int] -> IO [Int]
bogoSort [] = return []
bogoSort xs = do
    if isSorted xs
        then return xs
        else shuffle xs >>= bogoSort

dice :: IO Int
dice = getStdRandom $ randomR (1, 6)

showDice :: IO Int
showDice = do
    dice >>= print
    dice >>= return

-- Control.Applicativeを使ったアクション
fib :: Int -> IO Int
fib 0 = return 0
fib 1 = return 1
fib n | n > 0 = (+) <$> fib (n - 1) <*> fib (n - 2)

factForDebugByTrace :: Int -> Int
factForDebugByTrace 0 = trace "fact 0 = 1" 1
factForDebugByTrace n | n > 0 = trace dbg1 $ trace dbg2 ret
    where
        ret = n * fn1
        fn1 = factForDebugByTrace (n - 1)
        dbg1 = "fact " ++ show n ++ " = " ++ show n ++ " * fact " ++ show (n - 1)
        dbg2 = dbg1 ++ " = " ++ show n ++ " * " ++ show fn1 ++ " = " ++ show ret

factForDebugByPutStrLn :: Int -> IO Int
factForDebugByPutStrLn 0 = do
    putStrLn "fact 0 = 1"
    return 1
factForDebugByPutStrLn n = do
    let dbg1 = "fact " ++ show n ++ " = " ++ show n ++ " * fact " ++ show (n - 1)
    putStrLn dbg1
    n' <- factForDebugByPutStrLn (n - 1)
    let ret = n * n'
    putStrLn $ dbg1 ++ " = " ++ show n ++ " * " ++ show n' ++ " = " ++ show ret
    return ret

-- Debug.Traceを使ったデバッグ
qsortForDebugByTrace :: (Eq a, Ord a, Show a) => [a] -> [a]
qsortForDebugByTrace [] = []
qsortForDebugByTrace (p:xs) = trace dbg $ qsortForDebugByTrace lt ++ [p] ++ qsortForDebugByTrace ge
    where
        dbg = "qsort " ++ show (p:xs) ++ " = " ++ "qsort " ++ show lt ++ " ++ "
              ++ "[" ++ show p ++ "]" ++ " ++ " ++ "qsort " ++ show ge
        lt = [x | x <- xs, x < p]
        ge = [x | x <- xs, x >= p]

-- putStrLnでデバッグ
qsortForDebugByPutStrLn :: (Eq a, Ord a, Show a) => [a] -> IO [a]
qsortForDebugByPutStrLn [] = return []
qsortForDebugByPutStrLn (p:xs) = do
    let lt = [x | x <- xs, x < p]
        ge = [x | x <- xs, x >= p]
        dbg = "qsort " ++ show (p:xs) ++ " = " ++ "qsort " ++ show lt ++ " ++ " ++ "[" ++ show p ++ "]" ++ " ++ " ++ "qsort " ++ show ge
    putStrLn dbg
    lt' <- qsortForDebugByPutStrLn lt
    ge' <- qsortForDebugByPutStrLn ge
    return $ lt' ++ [p] ++ ge'

-- 変数の参照している値を変更するcounterの実装
counterByIORef :: IO (IO Int)
counterByIORef = do
    c <- newIORef 0
    return $ do
        b <- readIORef c
        writeIORef c (b + 1)
        readIORef c

-- IORefによる手続き的な繰り返しの実現（和の計算）
loopByIORef :: IO ()
loopByIORef = do
    s <- newIORef 0
    let loop n | n <= 100 = do
            s' <- readIORef s
            writeIORef s (s' + n)
            loop (n + 1)
        loop _ = readIORef s >>= print
    loop 0

-- 再帰を使った和の計算
loopWithIORefRemoved :: Int
loopWithIORefRemoved = loop 0 0
    where
        loop :: Int -> Int -> Int
        loop s n
            | n <= 100 = loop (s + n) (n + 1)
            | otherwise = s

sampleIOUArray :: IO ()
sampleIOUArray = do
    a <- newArray (0, 2) 0 :: IO (IOUArray Int Int)
    getElems a >>= print
    writeArray a 0 3
    getElems a >>= print
    writeArray a 1 6
    getElems a >>= print
    writeArray a 2 7
    getElems a >>= print

interpreterOfBrainFuck :: IO ()
interpreterOfBrainFuck = do
    let bf = ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<" ++
             ".>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[<++" ++
             "++>-]<+.[-]++++++++++."
    let lenbf = length bf
    jmp <- newArray (0, lenbf - 1) 0 :: IO (IOUArray Int Int)
    let loop :: Int -> [Int] -> IO ()
        loop i loops | i <= lenbf - 1 =
            case bf !! i of
                '[' -> loop (i + 1) (i:loops)
                ']' -> do
                    let (start:loops') = loops
                    writeArray jmp start i
                    writeArray jmp i start
                    loop (i + 1) loops'
                otherwise -> loop (i + 1) loops
        loop _ _ = return ()
    loop 0 []
    m <- newArray (0, 29999) 0 :: IO (IOUArray Int Word8)
    let loop :: Int -> Int -> IO ()
        loop pc r | pc < lenbf = case bf !! pc of
            '+' -> do
                v <- readArray m r
                writeArray m r (v + 1)
                loop (pc + 1) r
            '-' -> do
                v <- readArray m r
                writeArray m r (v - 1)
                loop (pc + 1) r
            '>' -> loop (pc + 1) (r + 1)
            '<' -> loop (pc + 1) (r - 1)
            '.' -> do
                v <- readArray m r
                putChar $ chr $ fromIntegral v
                loop (pc + 1) r
            '[' -> do
                v <- readArray m r
                if v == 0
                    then do
                        pc' <- readArray jmp pc
                        loop (pc' + 1) r
                    else loop (pc + 1) r
            ']' -> do
                v <- readArray m r
                if v /= 0
                    then do
                        pc' <- readArray jmp pc
                        loop (pc' + 1) r
                    else loop (pc + 1) r
            otherwise -> loop (pc + 1) r
        loop _ _ = return ()
    loop 0 0

-- unsafePerfromIOの例（参照透過性を破壊するので使わない）
factByUnsafePerfromIO :: Int -> Int
factByUnsafePerfromIO 0 = unsafePerformIO $ do
    putStrLn "fact 0 = 1"
    return 1
factByUnsafePerfromIO n | n > 0 = unsafePerformIO $ do
    let dbg = "fact " ++ show n ++ " = " ++ show n ++ " * fact " ++ show (n - 1)
    putStrLn dbg
    let n' = factByUnsafePerfromIO (n - 1)
    let ret = n * n'
    putStrLn $ dbg ++ " = " ++ show n ++ " * " ++ show n' ++ " = " ++ show ret
    return ret

result_practice07 :: IO ()
result_practice07 = do
    putStrLn "description:\n\tExample of Action.\n"
    putStr "randomAlpha = "
    randomAlpha >>= print
    putStr "untilLowerCaseZ = "
    untilLowerCaseZ
    putStr "factByAction 10 = "
    factByAction 10 >>= print
    putStr "shuffle [1..9] = "
    list <- shuffle [1..9]
    print list
    putStr "bogoSort "
    putStr $ show list
    putStr " = "
    bogoSort [1..9] >>= print
    putStr "showDice = "
    showDice
    -- putStr "print <<= showDice = "
    -- print =<< showDice
    putStr "fibByAction 6 = "
    fib 6 >>= print
    putStrLn "<<<<<<<<<< factForDebugByTrace 10 >>>>>>>>>>"
    traceIO $ show $ factForDebugByTrace 10
    putStrLn ""
    putStrLn "<<<<<<<<<< factForDebugByPutStrLn 10 >>>>>>>>>>"
    factForDebugByPutStrLn 10 >>= print
    putStrLn ""
    putStrLn "<<<<<<<<<< qsortForDebugByTrace list >>>>>>>>>>"
    putStr "list = "
    print list
    traceIO $ show $ qsortForDebugByTrace list
    putStrLn ""
    putStrLn "<<<<<<<<<< qsortForDebugByPutStrLn list >>>>>>>>>>"
    putStr "list = "
    print list
    qsortForDebugByPutStrLn list >>= print
    putStrLn ""
    putStrLn "<<<<<<<<<< counterByIORef >>>>>>>>>>"
    count <- counterByIORef
    putStr "1: "
    count >>= print
    putStr "2: "
    count >>= print
    putStr "3: "
    count >>= print
    putStrLn ""
    putStr "loopByIORef = "
    loopByIORef
    putStr "loopWithIORefRemoved = "
    print loopWithIORefRemoved
    putStrLn ""
    putStrLn "<<<<<<<<<< sampleIOUArray >>>>>>>>>>"
    sampleIOUArray
    putStrLn ""
    putStrLn "<<<<<<<<<< interpreterOfBrainF_ck >>>>>>>>>>"
    interpreterOfBrainFuck
    putStrLn ""
    putStr "<<<<<<<<<< factByUnsafePerfromIO 10 >>>>>>>>>>"
    print $ factByUnsafePerfromIO 10
