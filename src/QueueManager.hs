module QueueManager (run) where

import Bluefin.Eff
import Bluefin.State
import Broadcast
import Control.Monad (forever, when)
import Queue
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
            isQueueFull <- getIsQueueFull storeRead
            when isQueueFull do
                queueMaxSize <- getQueueMaxSize storeRead
                currentSize <- queueLength queue
                when (currentSize <= queueMaxSize `div` 2) do
                    putIsQueueFull storeWrite False
            -- TODO store event
            -- TODO do better here
            -- TODO check max size
            sleepMs sleep 50
