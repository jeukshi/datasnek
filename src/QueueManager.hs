module QueueManager (run) where

import Bluefin.Eff
import Bluefin.State
import Control.Monad (forever, when)
import Queue
import Sleep
import Store
import Types

run
    :: (e1 :> es, e2 :> es, e3 :> es, e4 :> es)
    => StoreWrite e1
    -> StoreRead e2
    -> Queue User e3
    -> Sleep e4
    -> Eff es ()
run storeWrite storeRead queue sleep = do
    initQueueMaxSize <- (.queueMaxSize) <$> getSettings storeRead
    evalState initQueueMaxSize \_ -> do
        forever do
            getNewPlayer storeRead >>= \case
                Just _ -> pure ()
                Nothing ->
                    tryReadQueue queue >>= \case
                        Nothing -> pure ()
                        user -> putNewPlayer storeWrite user
            -- TODO resize queue here
            queueMaxSize <- (.queueMaxSize) <$> getSettings storeRead
            queueCurrentSize <- queueLength queue
            getIsQueueFull storeRead >>= \case
                True -> do
                    when (queueCurrentSize <= queueMaxSize `div` 2) do
                        putIsQueueFull storeWrite False
                False -> when (queueCurrentSize == queueMaxSize) do
                    putIsQueueFull storeWrite True
            sleepMs sleep 50
