{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FieldSelectors #-}

module Broadcast where

import Bluefin.Compound (Handle (mapHandle))
import Bluefin.Eff (Eff, (:&), (:>))
import Bluefin.Extra (ThreadSafe (..), useImplIn2)
import Bluefin.IO (IOE, effIO)
import Bluefin.Stream (Stream, yield)
import Control.Concurrent.Chan.Unagi (
    InChan,
    dupChan,
    newChan,
    readChan,
    writeChan,
 )
import Control.Monad (forever)
import Store

data BroadcastClient a e = UnsafeMkBroadcastClient
    { ioe :: IOE e
    , inChan :: InChan a
    , store :: StoreRead e
    }

instance Handle (BroadcastClient a) where
    mapHandle c =
        UnsafeMkBroadcastClient
            { ioe = mapHandle c.ioe
            , inChan = c.inChan
            , store = mapHandle c.store
            }

instance ThreadSafe (BroadcastClient a) where
    accessConcurrently access (UnsafeMkBroadcastClient io inChan store) = do
        localIo <- accessConcurrently access io
        localStore <- accessConcurrently access store
        pure
            UnsafeMkBroadcastClient
                { ioe = localIo
                , store = localStore
                , inChan = inChan
                }

data BroadcastServer a e = UnsafeMkBroadcastServer
    { ioe :: IOE e
    , inChan :: InChan a
    }

-- Is it thread safe, though?
-- Fortunately, we have customers to test that in protoduction.
instance ThreadSafe (BroadcastServer a) where
    accessConcurrently access (UnsafeMkBroadcastServer io inChan) =
        UnsafeMkBroadcastServer <$> accessConcurrently access io <*> pure inChan

runBroadcast
    :: (e1 :> es, e2 :> es)
    => IOE e1
    -> StoreRead e2
    -> (forall e. BroadcastClient a e -> BroadcastServer a e -> Eff (e :& es) r)
    -> Eff es r
runBroadcast io store action = do
    (iChan, _) <- effIO io newChan
    let broadcastOut = UnsafeMkBroadcastServer (mapHandle io) iChan
    let broadcast = UnsafeMkBroadcastClient (mapHandle io) iChan (mapHandle store)
    useImplIn2 action broadcast broadcastOut

writeBroadcast
    :: (e :> es)
    => BroadcastServer a e
    -> a
    -> Eff es ()
writeBroadcast (UnsafeMkBroadcastServer io inChan) event =
    effIO io do writeChan inChan event

likeAndSubscribe
    :: (e :> es, e1 :> es)
    => BroadcastClient a e
    -> Stream a e1
    -> Eff es ()
likeAndSubscribe (UnsafeMkBroadcastClient io inChan _) s = do
    outChan <- effIO io do dupChan inChan
    forever do
        event <- effIO io do readChan outChan
        yield s event
