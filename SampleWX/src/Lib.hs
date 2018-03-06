module Lib
    ( sampleWX
    , testChoices
    , sampleChoices
    , sampleReactiveChoices
    ) where

import Control.Monad
import Control.Applicative
import Control.Event.Handler
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

sampleWX :: IO ()
sampleWX = start $ do
    f <- frame [text := "Sample"]
    let x = margin 10 $ column 5 [ boxed "coordinates" (grid 5 5 [[label "x", label "x'"]
                                                     ,[label "y", label "y'"]])
                     , alignBottomRight $ row 5 [label "ok", label "cansel"]]
    set f [layout := x]
    return ()

testChoices :: IO ()
testChoices = start $ do
    f <- frame [text := "testChoice"]
    c <- choice f [text := "--"]
    mapM (itemAppend c) $ map show [1..10]
    set f [layout := widget c]
    return ()

sampleChoices :: IO ()
sampleChoices = start $ do
    f <- frame [text := "SampleChoice"]
    ls <- sequence $ map (\x -> choice f [items := map show x]) [[1..10], [1..10]]
    _ <- sequence $ map (\l -> set l [on select := eventImpl l]) ls
    set f [layout := margin 10 $ column 5 (map widget ls)]
    return ()
    where
        eventImpl :: Choice () -> IO ()
        eventImpl c = do
            selected <- get c selection
            putStrLn $ "occur Event: " ++ show selected
            return ()

sampleReactiveChoices :: IO ()
sampleReactiveChoices = start $ do
    f <- frame [text := "sampleReactiveChoices"]
    cs <- sequence $ map (\x -> choice f [items := map show x]) [[0..10], [0..10]]
    tmp <- get (cs !! 0) text
    print tmp
    o <- staticText f []
    _ <- set f [layout := column 10 $ map widget cs ++ [widget o]]

    let netWorkDescription :: MomentIO ()
        netWorkDescription = do
            echoices <- sequence $ map (\c -> event0 c select) cs
            bchoices <- sequence $ map (\c -> behavior c selection) cs
            let result :: Behavior Int
                result = (+) <$> bchoices !! 0 <*> bchoices !! 1

            sink o [text :== show <$> result]

    network <- compile netWorkDescription
    actuate network
