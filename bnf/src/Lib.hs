module Lib
    ( SimpleDate(..)
    , Gender(..)
    , Profile(..)
    ) where

import Data.Text

data SimpleDate = SimpleDate
    { year  :: Int
    , month :: Int
    , day   :: Int
    } deriving (Eq, Ord, Show, Read)

data Gender = Male | Female | Unknown
    deriving (Eq, Enum, Show, Read)

data Profile = Profile
    { name :: Text
    , birthday :: SimpleDate
    , gender :: Gender
    } deriving (Eq, Show, Read)
