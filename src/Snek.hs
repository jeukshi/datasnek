module Snek where

import Command
import Data.Strict.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import User

type Sneks = [Snek]

data Snek = MkSnek
    { user :: User
    , color :: Text
    , headOfSnek :: (Int, Int)
    , restOfSnek :: [(Int, Int)]
    , gracePeriod :: Int
    }
    deriving (Show, Eq)

type SneksDirections = Map UserId SnekDirection

data SnekDirection = MkSnekDirection
    { current :: Direction
    , new :: Maybe Direction
    }
    deriving (Show, Generic)
