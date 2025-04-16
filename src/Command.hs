module Command where

import Data.Text (Text)
import Data.Text qualified as T
import Message
import Servant (FromHttpApiData (..))
import User

data Command
    = ChangeDirection User Direction
    | PlayRequest User
    | NewComment User Message
    deriving (Eq)

data Direction = U | D | L | R
    deriving (Eq, Show)

instance FromHttpApiData Direction where
    parseUrlPiece t = case T.toLower t of
        "u" -> Right U
        "d" -> Right D
        "l" -> Right L
        "r" -> Right R
        _ -> Left $ "Invalid direction: " <> t
