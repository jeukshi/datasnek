{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FieldSelectors #-}

module Broadcast where

import Bluefin.Compound (Handle (..), makeOp, useImpl, useImplIn, useImplUnder)
import Bluefin.Concurrent.Local qualified as BC
import Bluefin.Eff (Eff, runEff, (:&), (:>))
import Bluefin.Extra (ThreadSafe (..), useImplIn2)
import Bluefin.IO (IOE, effIO)
import Bluefin.Internal qualified
import Bluefin.Stream (Stream, forEach, yield)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan.Unagi (
    InChan,
    dupChan,
    getChanContents,
    newChan,
    readChan,
    writeChan,
 )
import Control.Monad (forever)
import Data.Foldable (for_)
import RawSse
import Settings (renderSettingsToRawEvent)
import Store
import StoreUpdate (StoreUpdate (..))
import Unsafe.Coerce (unsafeCoerce)

data BroadcastClient a e = UnsafeMkBroadcastClient
    { ioe :: IOE e
    , inChan :: InChan a
    , store :: StoreRead e
    , beforeRead :: forall e'. StoreRead e' -> Eff (e' :& e) [a]
    }

instance Handle (BroadcastClient a) where
    mapHandle c =
        UnsafeMkBroadcastClient
            { ioe = mapHandle c.ioe
            , inChan = c.inChan
            , store = mapHandle c.store
            , beforeRead = useImplUnder . beforeRead c
            }

instance ThreadSafe (BroadcastClient a) where
    accessConcurrently access bc@(UnsafeMkBroadcastClient io inChan store before) = do
        localIo <- accessConcurrently access io
        localStore <- accessConcurrently access store
        pure
            UnsafeMkBroadcastClient
                { -- FIXME I don't know how to write it.
                  beforeRead = makeOp . beforeRead (unsafeCoerce bc) . mapHandle
                , ioe = localIo
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
    let broadcast = UnsafeMkBroadcastClient (mapHandle io) iChan (mapHandle store) (\_ -> pure [])
    useImplIn2 action broadcast broadcastOut

runBroadcastStore
    :: (e1 :> es, e2 :> es)
    => IOE e1
    -> StoreRead e2
    -> (forall e. BroadcastClient StoreUpdate e -> BroadcastServer StoreUpdate e -> Eff (e :& es) r)
    -> Eff es r
runBroadcastStore io store action = do
    (iChan, _) <- effIO io newChan
    let broadcastOut = UnsafeMkBroadcastServer (mapHandle io) iChan
    let broadcast = UnsafeMkBroadcastClient (mapHandle io) iChan (mapHandle store) readStore
    useImplIn2 action broadcast broadcastOut
  where
    readStore :: (e' :> es') => StoreRead e' -> Eff es' [StoreUpdate]
    readStore store' = do
        -- TODO I don't like having this logic here.
        -- We don't do game frame here, but it will come fast enough.
        -- Also we do it, cuz we can keep our main page in cache and constant.
        -- But it seems less and less worth the truble.
        leaderboardFrame <- getLeaderboardFrame store'
        chatFrame <- getChatContent store'
        settingsFrame <- renderSettingsToRawEvent store'
        disableChat <- getDisableChat store'
        let chatInputRawEvent = renderChatInput disableChat
        pure
            [ LeaderboardFrameUpdate leaderboardFrame
            , ChatFrameUpdate chatFrame
            , SettingsFrameUpdate settingsFrame
            , ChatInputUpdate chatInputRawEvent
            ]
    renderChatInput disabled =
        MkRawEvent
            ( "event:datastar-merge-signals\n"
                <> "data:signals {showchat: "
                <> if disabled then "false" else "true" <> "}\n"
            )

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
likeAndSubscribe bc@(UnsafeMkBroadcastClient io inChan store readInitial) s = do
    outChan <- effIO io do dupChan inChan
    initialEvents <- makeOp (beforeRead (mapHandle bc) (mapHandle store))
    for_ initialEvents \event -> do
        yield s event
    forever do
        event <- effIO io do readChan outChan
        yield s event
