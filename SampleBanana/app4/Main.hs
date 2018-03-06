{-# LANGUAGE RecursiveDo #-}

module Main
    ( main
    ) where


import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Event.Handler
import Control.Monad

main :: IO ()
main = do
    (addHandler, runHandler) <- newAddHandler

    let networkDesctiption :: MomentIO ()
        networkDesctiption = mdo
            e <- fromAddHandler addHandler
            -- b <- fromChanges 0 addHandler
            b' <- stepper 0 e'
            let e' = (+) <$> b' <@> e
            reactimate $ (\x -> putStrLn $ show x) <$> e'
            reactimate $ (\x -> putStrLn $ replicate x '*') <$> e

    network <- compile networkDesctiption
    actuate network

    forever $ fmap read getLine >>= runHandler
