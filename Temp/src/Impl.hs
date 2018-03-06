module Impl where

import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

type DOW = String
type LectureNumber = Int

run :: IO ()
run = start $ do
    f <- frame [text := "Temporary"] :: IO (Frame ())
    input <- sequence $ replicate 7 $ sequence $ replicate 5 $ entry f [] :: IO [[TextCtrl ()]]
    let widgets = map (fmap widget) input :: [[Layout]]
        dow = map label ["Mon.", "Tue.", "Wed.", "Thu.", "Fri", "Sat.", "Sun"] :: [Layout]
        hour = map label ["", "1", "2", "3", "4", "5"] :: [Layout]
        newWidgets = zipWith (\x y -> x : y) dow widgets
        columns = map (column 5) newWidgets :: [Layout]

        rows = row 5 (hour : columns)

    set f [layout := margin 10 rows]

-- oneWeek :: Frame () -> LectureNumber -> Layout
-- oneWeek f n = row 5 $ map (oneDay f n) ["Mon.", "Tue.", "Wed.", "Thu.", "Fri", "Sat.", "Sun"]
