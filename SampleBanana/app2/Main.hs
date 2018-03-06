module Main
    ( main
    ) where


import Control.Applicative
import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Event.Handler
import Graphics.UI.WX  hiding (Event)

main :: IO ()
main = undefined

-- networkDesctiption :: Button () -> TextCtrl () -> MomentIO ()
-- networkDesctiption button text = do
--     ebutton <- button `event0` command
--     btext <- text `behavior` text
--     let result = fmap (\_ -> "Hello Banana!!") btext
--     sink text [text :== result]
