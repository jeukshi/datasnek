module GameState where

import Data.Strict.Set (Set)
import Snek (Snek, SnekDirection, Sneks)

data GameState = MkGameState
    { boardSize :: Int
    , foodPositions :: Set (Int, Int)
    , aliveSneks :: Sneks
    , newPlayer :: Maybe (Snek, SnekDirection)
    , maxPlayers :: Int
    }
    deriving (Show)
