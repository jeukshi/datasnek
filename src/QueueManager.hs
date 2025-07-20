module QueueManager (run) where

import Bluefin.Eff
import Bluefin.IO (effIO)
import Bluefin.Internal qualified
import Bluefin.State
import Broadcast
import Control.Monad (forever, when)
import Queue
import RawSse (RawEvent (..))
import Sleep
import Store
import StoreUpdate
import User

run
    :: (e1 :> es, e2 :> es, e3 :> es, e4 :> es, e5 :> es)
    => StoreWrite e1
    -> StoreRead e2
    -> Queue User e3
    -> BroadcastServer StoreUpdate e4
    -> Sleep e5
    -> Eff es ()
run storeWrite storeRead queue broadcastServer sleep = do
    initQueueMaxSize <- getQueueMaxSize storeRead
    evalState initQueueMaxSize \queueSize -> do
        forever do
            getNewPlayer storeRead >>= \case
                Just _ -> pure ()
                Nothing ->
                    tryReadQueue queue >>= \case
                        Nothing -> pure ()
                        user -> putNewPlayer storeWrite user
            -- TODO resize queue here
            queueMaxSize <- getQueueMaxSize storeRead
            queueCurrentSize <- queueLength queue
            getIsQueueFull storeRead >>= \case
                True -> do
                    when (queueCurrentSize <= queueMaxSize `div` 2) do
                        putIsQueueFull storeWrite False
                        writeBroadcast broadcastServer $ QueueUpdate (queueEvent False)
                False -> when (queueCurrentSize == queueMaxSize) do
                    putIsQueueFull storeWrite True
                    writeBroadcast broadcastServer $ QueueUpdate (queueEvent True)
            sleepMs sleep 50
  where
    queueEvent isFull =
        MkRawEvent
            ( "event:datastar-patch-signals\n"
                <> "data:signals {queuefull: '"
                <> (if isFull then "true" else "false")
                <> "'}\n"
            )
