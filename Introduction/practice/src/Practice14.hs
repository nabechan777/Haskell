{-
    練習問題（モナド変換子）
    ・Identityモナド
        <- 値だけを持っているモナド
        <- 恒等モナドと言われる。
        <- 要素数は1
        <- 内部関数も持っていない。

    ・StateTモナド変換子
        <- StateT s m a
        <- mは任意のモナドを指定する。
        <- type State s a = StateT s Identity aとなっている。
        <- StateTモナド変換子：s -> m (a, s) （内部関数）
        <- Stateモナド：、s -> Identity (a, s) （内部関数の実体）
        <- runStateT :: StateT s m a -> s -> m (a, s)（内部関数の取り出し）
        <- evalStateT :: Monad m => StateT s m a -> s -> m a（値の取り出し）
        <- execStateT :: Monad m => StateT s m a -> s -> m s（状態の取り出し）

    ・モナドの合成
        <- StateTモナド変換子は他のモナドを入れることができる。
        <- Identityモナドの代わりにIOモナドなども入れることができる。
        <- つまり、Identityモナド以外を扱う内部関数も入れることができる。
        <- 素直に内部関数を定義する方法とliftを使う方法がある。
        <- lift :: Monad m => m a -> t m a

-}

module Practice14
    ( result_practice14
    ) where

import Control.Monad.Identity
import Control.Monad.State

sampleIdentity :: IO ()
sampleIdentity = do
    let a = return 1 :: Identity Int
    print $ runIdentity a

sampleRunStateT :: IO ()
sampleRunStateT = do
    let st = return 1 :: State s Int
    putStr "runState st = "
    print $ runState st ()
    putStr "runStateT st = "
    print $ runIdentity $ runStateT st ()

returnForStateByStateT :: a -> StateT s Identity a
returnForStateByStateT x = StateT $ \s -> Identity (x, s)

runStatebyStateT :: StateT s Identity a -> s -> (a, s)
runStatebyStateT st = runIdentity . runStateT st

result_practice14 :: IO ()
result_practice14 = do
    putStrLn "description:\n\tExsample of Monad Transformer.\n"
    putStr "sampleIdentity = "
    sampleIdentity
    sampleRunStateT
    let a = return 1 :: StateT s Identity Int
    putStr "runStateT a = "
    print $ runIdentity $ runStateT a ()
    putStr "evalStateT a = "
    print $ runIdentity $ evalStateT a ()
    putStr "execStateT a = "
    print $ runIdentity $ execStateT a ()
    putStr "runStatebyStateT (returnForStateByStateT 2) () = "
    print $ runStatebyStateT (returnForStateByStateT 2) ()
