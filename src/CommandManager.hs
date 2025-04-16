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
import SnekWebComponent
import Store
import StoreUpdate
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
        ChangeDirection user newDirection -> do
            snd <$> atomicModifySnekDirection storeWrite \sneks -> do
                -- TODO allowed directions
                let newSneks =
                        Map.adjust
                            (\sd -> MkSnekDirection sd.current (Just newDirection))
                            user.userId
                            sneks
                pure (newSneks, ())
        PlayRequest user -> do
            -- Making sure we don't block here.
            _ <- BC.fork scope \acc -> do
                localGameQueue <- accessConcurrently acc gameQueue
                writeQueue localGameQueue user
            pure ()
        NewComment user message -> do
            -- Making sure we don't block here.
            _ <- BC.fork scope \acc -> do
                localChatQueue <- accessConcurrently acc chatQueue
                writeQueue localChatQueue (user, message)
            pure ()
