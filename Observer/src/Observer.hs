module Observer
    ( Observers (..)
    , update
    )where

data Observers = DigitObserver
               | GraphObserver

update :: (Integral a, Show a) => Observers -> (a -> IO ())
update DigitObserver = (\x -> putStrLn $ "DigitObserver: " ++ show x)
update GraphObserver = (\x -> putStrLn $ "GraphObserver: " ++ replicate (fromIntegral x) '*')
update _             = error "Undefined Observer"
