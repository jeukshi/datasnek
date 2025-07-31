module CommandManager (run) where

import Bluefin.Concurrent.Local qualified as BC
import Bluefin.Eff (Eff, (:>))
import Bluefin.Extra (ThreadSafe (accessConcurrently))
import Bluefin.Stream (forEach)
import Broadcast (BroadcastClient, likeAndSubscribe)
import Control.Monad (when)
import Data.Strict.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Queue (Queue, writeQueue)
import Store (
    StoreRead,
    StoreWrite,
    atomicModifySnekDirection,
    getSettings,
    putSettings,
 )
import Text.Read qualified as T
import Types (
    ChatCanChange (..),
    ChatMode (..),
    Command (..),
    Direction (..),
    Message (..),
    Settings (..),
    SnekDirection (..),
    User,
 )

run
    :: ( e1 :> es
       , e2 :> es
       , e5 :> es
       , e6 :> es
       , e4 :> scopeEs
       , e3 :> scopeEs
       )
    => StoreWrite e1
    -> StoreRead e2
    -> Queue User e3
    -> Queue (User, Message) e4
    -> BC.Scope scopeEs e5
    -> BroadcastClient Command e6
    -> Eff es ()
run storeWrite storeRead gameQueue chatQueue scope broadcastCommandClient =
    forEach (likeAndSubscribe broadcastCommandClient) \case
        ChangeDirection userId newDirection -> do
            snekCanReverse <- (.snekCanReverse) <$> getSettings storeRead
            snd <$> atomicModifySnekDirection storeWrite \sneks -> do
                -- TODO allowed directions
                let newSneks =
                        Map.adjust
                            ( \sd ->
                                MkSnekDirection
                                    sd.current
                                    (pickNewDirection snekCanReverse newDirection sd.current sd.new)
                            )
                            userId
                            sneks
                pure (newSneks, ())
        PlayRequest user -> do
            -- Making sure we don't block here.
            _ <- BC.fork scope \acc -> do
                localGameQueue <- accessConcurrently acc gameQueue
                writeQueue localGameQueue user
            pure ()
        NewComment user message -> do
            commandResult <-
                if "!" `T.isPrefixOf` message.text
                    then do
                        settings <- getSettings storeRead
                        executeCommand storeWrite settings message.text
                    else pure False
            chatMode <- (.chatMode) <$> getSettings storeRead
            case (chatMode, commandResult) of
                (ChatOn, _) -> passMessageToChat user message
                (ChatOff, _) -> passMessageToChat user message
                (ChatCommands, True) -> passMessageToChat user message
                _ -> pure ()
  where
    passMessageToChat user message = do
        -- Making sure we don't block here.
        _ <- BC.fork scope \acc -> do
            localChatQueue <- accessConcurrently acc chatQueue
            writeQueue localChatQueue (user, message)
        pure ()

    pickNewDirection :: Bool -> Direction -> Direction -> Maybe Direction -> Maybe Direction
    pickNewDirection = \cases
        True newDirection _ _ -> Just newDirection
        False U D currNewDirection -> currNewDirection
        False D U currNewDirection -> currNewDirection
        False L R currNewDirection -> currNewDirection
        False R L currNewDirection -> currNewDirection
        False newDirection _ _ -> Just newDirection

data PossiblyACommand
    = CommandBool Text Bool
    | CommandInt Text Int
    | NotReally
    deriving stock (Show)

executeCommand :: (e :> es) => StoreWrite e -> Settings -> Text -> Eff es Bool
executeCommand storeWrite settings text = do
    let ccc = settings.chatCanChange
    case parseCommand text of
        NotReally -> pure False
        (CommandInt "!maxBots" int) -> do
            when (between int ccc.botsMin ccc.botsMax) do
                putSettings storeWrite settings{maxBots = int}
            pure True
        (CommandInt "!maxPlayers" int) -> do
            when (between int ccc.playersMin ccc.playersMax) do
                putSettings storeWrite settings{maxPlayers = int}
            pure True
        (CommandInt "!maxFood" int) -> do
            when (between int ccc.foodMin ccc.foodMax) do
                putSettings storeWrite settings{maxFood = int}
            pure True
        (CommandInt "!boardSize" int) -> do
            if settings.useWebComponent
                then when (between int ccc.boardSizeWcMin ccc.boardSizeWcMax) do
                    putSettings storeWrite settings{boardSize = int}
                else when (between int ccc.boardSizeMin ccc.boardSizeMax) do
                    putSettings storeWrite settings{boardSize = int}
            pure True
        (CommandInt "!gracePeriod" int) -> do
            when (between int ccc.gracePeriodMin ccc.gracePeriodMax) do
                putSettings storeWrite settings{gracePeriod = int}
            pure True
        (CommandInt "!frameTimeMs" int) -> do
            when (between int ccc.frameTimeMsMin ccc.frameTimeMsMax) do
                putSettings storeWrite settings{frameTimeMs = int}
            pure True
        (CommandBool "!snekSelfOwn" bool) -> do
            when ccc.selfOwn do
                putSettings storeWrite settings{snekSelfOwn = bool}
            pure True
        (CommandBool "!snekCanReverse" bool) -> do
            when ccc.canReverse do
                putSettings storeWrite settings{snekCanReverse = bool}
            pure True
        (CommandBool "!useWebComponent" bool) -> do
            when ccc.webComponent do
                let minBs = if bool then ccc.boardSizeWcMin else ccc.boardSizeMin
                let maxBs = if bool then ccc.boardSizeWcMax else ccc.boardSizeMax
                putSettings
                    storeWrite
                    settings
                        { useWebComponent = bool
                        , boardSize = max minBs (min maxBs settings.boardSize)
                        }
            pure True
        _ -> pure False
  where
    parseCommand :: Text -> PossiblyACommand
    parseCommand someText = case T.splitOn " " someText of
        [command, mbValue] -> case mbValue of
            "false" -> CommandBool command False
            "true" -> CommandBool command True
            mbInt -> case T.readMaybe (T.unpack mbInt) of
                Just int -> CommandInt command int
                Nothing -> NotReally
        _ -> NotReally
    between val lo hi = val >= lo && val <= hi
