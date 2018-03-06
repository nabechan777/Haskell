module Subject
    ( Subjects (..)
    , notify
    )where

import Observer
import Control.Monad (mapM_)

data Subjects = NumberGenerator { get :: !Int, observers :: ![Observers] }

notify :: Subjects -> IO ()
notify (NumberGenerator v os) = mapM_ (`update` v) os
