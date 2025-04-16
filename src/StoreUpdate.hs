module StoreUpdate where

import Data.Strict.Map (Map)
import RawSse (RawEvent)
import Snek (SneksDirections)
import User (UserId)

data StoreUpdate
    = GameFrameUpdate (Map UserId RawEvent) RawEvent SneksDirections
    | WebComponentUpdate RawEvent SneksDirections
    | LeaderboardFrameUpdate RawEvent
    | ChatFrameUpdate RawEvent
    | ChatNewMessage RawEvent
    | SettingsFrameUpdate RawEvent
    | UsernameUpdate RawEvent
    | ChatInputUpdate RawEvent
