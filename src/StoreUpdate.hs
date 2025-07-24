module StoreUpdate where

import Data.Strict.Map (Map)
import RawSse (RawEvent)
import Snek (SneksDirections)
import User (UserId)

data StoreUpdate
    = GameFrameUpdate (Map UserId RawEvent) RawEvent SneksDirections
    | WebComponentUpdate RawEvent SneksDirections
    | ChatFrameUpdate RawEvent
    | ChatNewMessage RawEvent
    | UsernameUpdate RawEvent
    | ChatEnable RawEvent
    | ChatDisable RawEvent
