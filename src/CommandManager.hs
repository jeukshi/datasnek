module CommandManager (run) where

import Bluefin.Concurrent.Local qualified as BC
import Bluefin.Eff (Eff, (:>))
import Bluefin.Extra (ThreadSafe (accessConcurrently))
import Bluefin.IO (effIO)
import Bluefin.Internal qualified
import Bluefin.State
import Bluefin.StateSource (newState, withStateSource)
import Bluefin.Stream (forEach)
import Broadcast
import Color (assignColor)
import Command
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM qualified as STM
import Control.Monad (forever, when)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (for_)
import Data.Graph (components)
import Data.List (partition)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Strict.Map (Map)
import Data.Strict.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Traversable (for)
import Lucid hiding (for_)
import Lucid.Datastar (dataAttr_)
import Message
import Queue
import Random
import RawSse (RawEvent (..))
import Servant (FromHttpApiData (..))
import Snek
import Store
import StoreUpdate
import Text.ParserCombinators.ReadP qualified as P
import Text.Read qualified as T
import Types (ChatCanChange (..), ChatMode (..), Settings (..))
import Unsafe.Coerce (unsafeCoerce)
import User

run
    :: (e1 :> es, e2 :> es, e3 :> es, e4 :> es, e5 :> es, e6 :> es, e7 :> es, e4 :> scopeEs)
    => Random e1
    -> StoreWrite e2
    -> StoreRead e3
    -> Queue User e4
    -> Queue (User, Message) e4
    -> BC.Scope scopeEs e5
    -> BroadcastClient Command e6
    -> BroadcastServer StoreUpdate e7
    -> Eff es ()
run random storeWrite storeRead gameQueue chatQueue scope broadcastCommandClient broadcastGameStateServer =
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
                if "!" `T.isPrefixOf` message.unMessage
                    then do
                        settings <- getSettings storeRead
                        executeCommand storeWrite settings message.unMessage
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
    deriving (Show)

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
    parseCommand text = case T.splitOn " " text of
        [command, mbValue] -> case mbValue of
            "false" -> CommandBool command False
            "true" -> CommandBool command True
            mbInt -> case T.readMaybe (T.unpack mbInt) of
                Just int -> CommandInt command int
                Nothing -> NotReally
        _ -> NotReally
    between val min max = val >= min && val <= max
