module Main
    ( main
    ) where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Graphics.UI.WX hiding (Event)

main :: IO ()
main = start $ do
    f <- frame [text := "Sample3"]
    c <- choice f [items := "--" : map show xs]
    res <- staticText f []
    let action :: Choice () -> IO Int
        action c = get c selection >>= return . (\x -> if x /= 0 then (xs !!) . (subtract 1) $ x else 0)

    -- 選択メニューの処理
    (choiceAddHandler, choiceRunHandler) <- newAddHandler
    set c [on select := action c >>= choiceRunHandler]

    set f [layout := margin 10 $ column 10 [widget c, widget res]]

    let networkDesctiption :: MomentIO ()
        networkDesctiption = do
            -- AddHandlerからEvent Intを生成する。
            echoice <- fromAddHandler choiceAddHandler

            let
                -- Event IntをEvent Stringに変換する。
                echoice' = fmap (\x -> if x == 0 then "" else show x) echoice

            -- Event StringからBehavior Stringを生成する。
            bchoice <- stepper "" echoice'

            -- Behaviorの値を変更する。
            changeBehavior <- changes bchoice
            reactimate' $ (fmap (\x -> set res [text := x])) <$> changeBehavior

    network <- compile networkDesctiption
    actuate network
    return ()
    where
        xs :: [Int]
        xs = [1..10]
