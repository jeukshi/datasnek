{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Store where

import Bluefin.Compound (Handle (..))
import Bluefin.Concurrent.Local qualified as BC
import Bluefin.Eff (Eff, (:&), (:>))
import Bluefin.Extra
import Bluefin.IO (IOE, effIO)
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar, tryReadMVar)
import Control.Concurrent.STM (writeTVar)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar)
import Data.IORef
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Strict.Map qualified as Map
import Datastar qualified
import Html qualified
import Lucid (Html)
import RawSse (RawEvent (..))
import Types

-- General guidance here is that if something is an IORef,
-- it is written to by only one thread. If it is a MVar/TVar,
-- by more than one. This is not enforced by an effect system.
data Store = UnsafeMkStore
    { gameFrame :: IORef RawEvent
    , mainPage :: RenderedHtml
    , loginPage :: RenderedHtml
    , chatEnabled :: RawEvent
    , chatDisabled :: RawEvent
    , snekDirection :: MVar SneksDirections
    , sneks :: IORef Sneks
    , foodPositions :: IORef (Set (Int, Int))
    , newPlayer :: TVar (Maybe User)
    , isQueueFull :: IORef Bool
    , chatMessages :: IORef [(User, Message)]
    , chatContent :: IORef RawEvent
    , chatContentHtml :: IORef (Html ())
    , settings :: IORef Settings
    }

data StoreWrite e = UnsafeMkStoreWrite
    { ioe :: IOE e
    , stme :: BC.STME e
    , store :: Store
    }

-- It isn't, but okay.
instance ThreadSafe StoreWrite where
    accessConcurrently access (UnsafeMkStoreWrite io stme store) =
        UnsafeMkStoreWrite
            <$> accessConcurrently access io
            <*> accessConcurrently access stme
            <*> pure store

data StoreRead e = UnsafeMkStoreRead
    { ioe :: IOE e
    , stme :: BC.STME e
    , store :: Store
    }

instance Handle StoreRead where
    mapHandle c =
        UnsafeMkStoreRead
            { ioe = mapHandle c.ioe
            , stme = mapHandle c.stme
            , store = c.store
            }

-- It isn't, but okay.
instance ThreadSafe StoreRead where
    accessConcurrently access (UnsafeMkStoreRead io stme store) =
        UnsafeMkStoreRead
            <$> accessConcurrently access io
            <*> accessConcurrently access stme
            <*> pure store

data StoreChatRead e = UnsafeMkStoreChatRead
    { ioe :: IOE e
    , stme :: BC.STME e
    , store :: Store
    }

instance Handle StoreChatRead where
    mapHandle c =
        UnsafeMkStoreChatRead
            { ioe = mapHandle c.ioe
            , stme = mapHandle c.stme
            , store = c.store
            }

instance ThreadSafe StoreChatRead where
    accessConcurrently access (UnsafeMkStoreChatRead io stme store) =
        UnsafeMkStoreChatRead
            <$> accessConcurrently access io
            <*> accessConcurrently access stme
            <*> pure store

runStore
    :: (e1 :> es, e2 :> es)
    => IOE e1
    -> BC.STME e2
    -> Env
    -> (forall e. StoreRead e -> StoreWrite e -> StoreChatRead e -> Eff (e :& es) r)
    -> Eff es r
runStore io stme env action = do
    gameFrame <- effIO io do newIORef (MkRawEvent "") -- Nobody will notice.
    let mainPage = renderHtml $ Html.mainPage env
    let loginPage = renderHtml Html.loginPage
    let chatEnabled = Datastar.patchElements Html.chatEnabled
    let chatDisabled = Datastar.patchElements Html.chatDisabled
    snekDirection <- effIO io do newMVar Map.empty
    sneks <- effIO io do newIORef []
    settings <- effIO io do
        newIORef
            MkSettings
                { maxFood = 5
                , maxPlayers = 50
                , queueMaxSize = 50
                , boardSize = 40
                , frameTimeMs = 200
                , useWebComponent = True
                , anonymousMode = True
                , chatMode = ChatOff
                , maxBots = 40
                , gracePeriod = 10
                , snekSelfOwn = False
                , snekCanReverse = True
                , chatCanChange =
                    MkChatCanChange
                        { botsMin = 0
                        , botsMax = 100
                        , playersMin = 0
                        , playersMax = 100
                        , foodMin = 0
                        , foodMax = 50
                        , boardSizeMin = 5
                        , boardSizeMax = 200
                        , boardSizeWcMin = 5
                        , boardSizeWcMax = 400
                        , gracePeriodMin = 0
                        , gracePeriodMax = 100
                        , selfOwn = True
                        , canReverse = True
                        , webComponent = True
                        , frameTimeMsMin = 50
                        , frameTimeMsMax = 1000
                        }
                }
    foodPositions <- effIO io do newIORef Set.empty
    isQueueFull <- effIO io do newIORef False
    newPlayer <- effIO io do newTVarIO Nothing
    chatContent <- effIO io do newIORef (MkRawEvent "") -- Nobody will notice.
    chatHtml <- Html.chatMessages []
    chatContentHtml <- effIO io do newIORef chatHtml
    chatMessages <- effIO io do newIORef []
    let store =
            UnsafeMkStore
                { gameFrame = gameFrame
                , mainPage = mainPage
                , loginPage = loginPage
                , chatEnabled = chatEnabled
                , chatDisabled = chatDisabled
                , snekDirection = snekDirection
                , sneks = sneks
                , foodPositions = foodPositions
                , newPlayer = newPlayer
                , settings = settings
                , isQueueFull = isQueueFull
                , chatContent = chatContent
                , chatContentHtml = chatContentHtml
                , chatMessages = chatMessages
                }
    let storeWrite = UnsafeMkStoreWrite (mapHandle io) (mapHandle stme) store
    let storeRead = UnsafeMkStoreRead (mapHandle io) (mapHandle stme) store
    let storeChatRead = UnsafeMkStoreChatRead (mapHandle io) (mapHandle stme) store
    useImplIn3 action storeRead storeWrite storeChatRead

getGameFrame :: (e :> es) => StoreRead e -> Eff es RawEvent
getGameFrame (UnsafeMkStoreRead io _ store) =
    effIO io $ readIORef store.gameFrame

putGameFrame :: (e :> es) => StoreWrite e -> RawEvent -> Eff es ()
putGameFrame (UnsafeMkStoreWrite io _ store) =
    effIO io . writeIORef store.gameFrame

getMainPage :: (e :> es) => StoreRead e -> Eff es RenderedHtml
getMainPage (UnsafeMkStoreRead _ _ store) = return store.mainPage

getLoginPage :: (e :> es) => StoreRead e -> Eff es RenderedHtml
getLoginPage (UnsafeMkStoreRead _ _ store) = return store.loginPage

getChatEnabled :: (e :> es) => StoreRead e -> Eff es RawEvent
getChatEnabled (UnsafeMkStoreRead _ _ store) = return store.chatEnabled

getChatDisabled :: (e :> es) => StoreRead e -> Eff es RawEvent
getChatDisabled (UnsafeMkStoreRead _ _ store) = return store.chatDisabled

atomicModifySnekDirection
    :: (e :> es)
    => StoreWrite e
    -> (SneksDirections -> Eff es (SneksDirections, a))
    -> Eff es (SneksDirections, a)
atomicModifySnekDirection (UnsafeMkStoreWrite io _ store) f = do
    sneksDirection <- effIO io $ takeMVar store.snekDirection
    (newSneksDirections, res) <- f sneksDirection
    effIO io $ putMVar store.snekDirection newSneksDirections
    pure (newSneksDirections, res)

tryReadSneksDirections :: (e :> es) => StoreRead e -> Eff es (Maybe SneksDirections)
tryReadSneksDirections (UnsafeMkStoreRead io _ store) =
    effIO io $ tryReadMVar store.snekDirection

getSneks :: (e :> es) => StoreRead e -> Eff es Sneks
getSneks (UnsafeMkStoreRead io _ store) =
    effIO io $ readIORef store.sneks

putSneks :: (e :> es) => StoreWrite e -> Sneks -> Eff es ()
putSneks (UnsafeMkStoreWrite io _ store) =
    effIO io . writeIORef store.sneks

getFoodPositions :: (e :> es) => StoreRead e -> Eff es (Set (Int, Int))
getFoodPositions (UnsafeMkStoreRead io _ store) =
    effIO io $ readIORef store.foodPositions

putFoodPositions :: (e :> es) => StoreWrite e -> Set (Int, Int) -> Eff es ()
putFoodPositions (UnsafeMkStoreWrite io _ store) =
    effIO io . writeIORef store.foodPositions

getNewPlayer :: (e :> es) => StoreRead e -> Eff es (Maybe User)
getNewPlayer (UnsafeMkStoreRead _ stme store) =
    BC.atomicallySTM stme do readTVar store.newPlayer

putNewPlayer :: (e :> es) => StoreWrite e -> Maybe User -> Eff es ()
putNewPlayer (UnsafeMkStoreWrite _ stme store) user =
    BC.atomicallySTM stme do writeTVar store.newPlayer user

getIsQueueFull :: (e :> es) => StoreRead e -> Eff es Bool
getIsQueueFull (UnsafeMkStoreRead io _ store) =
    effIO io $ readIORef store.isQueueFull

putIsQueueFull :: (e :> es) => StoreWrite e -> Bool -> Eff es ()
putIsQueueFull (UnsafeMkStoreWrite io _ store) =
    effIO io . writeIORef store.isQueueFull

getChatMessages :: (e :> es) => StoreRead e -> Eff es [(User, Message)]
getChatMessages (UnsafeMkStoreRead io _ store) =
    effIO io $ readIORef store.chatMessages

putChatMessages :: (e :> es) => StoreWrite e -> [(User, Message)] -> Eff es ()
putChatMessages (UnsafeMkStoreWrite io _ store) =
    effIO io . writeIORef store.chatMessages

getChatContent :: (e :> es) => StoreRead e -> Eff es RawEvent
getChatContent (UnsafeMkStoreRead io _ store) =
    effIO io $ readIORef store.chatContent

putChatContent :: (e :> es) => StoreWrite e -> RawEvent -> Eff es ()
putChatContent (UnsafeMkStoreWrite io _ store) =
    effIO io . writeIORef store.chatContent

getChatContentHtml :: (e :> es) => StoreRead e -> Eff es (Html ())
getChatContentHtml (UnsafeMkStoreRead io _ store) =
    effIO io $ readIORef store.chatContentHtml

putChatContentHtml :: (e :> es) => StoreWrite e -> Html () -> Eff es ()
putChatContentHtml (UnsafeMkStoreWrite io _ store) =
    effIO io . writeIORef store.chatContentHtml

getSettings :: (e :> es) => StoreRead e -> Eff es Settings
getSettings (UnsafeMkStoreRead io _ store) =
    effIO io $ readIORef store.settings

putSettings :: (e :> es) => StoreWrite e -> Settings -> Eff es ()
putSettings (UnsafeMkStoreWrite io _ store) =
    effIO io . writeIORef store.settings

maybeChatEnabled :: (e :> es) => StoreChatRead e -> Eff es (Maybe RawEvent)
maybeChatEnabled (UnsafeMkStoreChatRead io _ store) = do
    settings <- effIO io do readIORef store.settings
    case settings.chatMode of
        ChatOff -> return Nothing
        ChatOn -> return $ Just store.chatEnabled
        ChatCommands -> return $ Just store.chatEnabled
