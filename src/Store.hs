{-# LANGUAGE DuplicateRecordFields #-}

module Store where

import Bluefin.Compound (Handle (..), useImplIn)
import Bluefin.Concurrent.Local qualified as BC
import Bluefin.Eff (Eff, (:&), (:>))
import Bluefin.Extra
import Bluefin.IO (IOE, effIO)
import Color (assignColor)
import Command (Direction (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar, tryReadMVar)
import Control.Concurrent.STM (writeTVar)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar)
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Strict.Map (Map)
import Data.Strict.Map qualified as Map
import JavaScript qualified
import Lucid (Html)
import Lucid.Datastar (dataBind_, dataOnKeydown_, dataShow_, dataSignals_)
import Lucid.Html5
import Message
import Numeric.Natural (Natural)
import RawSse (RawEvent (..))
import RenderHtml qualified
import Snek
import User

-- General guidance here is that if something is an IORef,
-- it is written to by only one thread. If it is a MVar/TVar,
-- by more than one. This is not enforced by an effect system.
data Store = UnsafeMkStore
    { gameFrame :: IORef RawEvent
    , leaderboardFrame :: IORef RawEvent
    , snekDirection :: MVar SneksDirections
    , sneks :: IORef Sneks
    , maxFood :: IORef Int
    , foodPositions :: IORef (Set (Int, Int))
    , newPlayer :: TVar (Maybe User)
    , maxPlayers :: IORef Int
    , isQueueFull :: IORef Bool
    , queueMaxSize :: IORef Natural
    , boardSize :: IORef Int
    , gameFrameTimeMs :: IORef Integer
    , renderWebComponent :: IORef Bool
    , chatMessages :: IORef [(User, Message)]
    , chatContent :: IORef RawEvent
    , chatContentHtml :: IORef (Html ())
    , disableChat :: IORef Bool
    , anonymousMode :: IORef Bool
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

runStore
    :: (e1 :> es, e2 :> es)
    => IOE e1
    -> BC.STME e2
    -> (forall e. StoreRead e -> StoreWrite e -> Eff (e :& es) r)
    -> Eff es r
runStore io stme action = do
    gameFrame <- effIO io do newIORef (MkRawEvent "") -- Nobody will notice.
    leaderboardFrame <- effIO io do newIORef (MkRawEvent "") -- Nobody will notice.
    snekDirection <- effIO io do newMVar Map.empty
    sneks <- effIO io do newIORef []
    maxFood <- effIO io do newIORef 5
    foodPositions <- effIO io do newIORef Set.empty
    isQueueFull <- effIO io do newIORef False
    queueMaxSize <- effIO io do newIORef 100
    maxPlayers <- effIO io do newIORef 20
    newPlayer <- effIO io do newTVarIO Nothing
    boardSize <- effIO io do newIORef 40
    gameFrameTimeMs <- effIO io do newIORef 200
    renderWebComponent <- effIO io do newIORef True
    chatContent <- effIO io do newIORef (MkRawEvent "") -- Nobody will notice.
    chatHtml <- RenderHtml.chatMessages []
    chatContentHtml <- effIO io do newIORef chatHtml
    chatMessages <- effIO io do newIORef []
    disableChat <- effIO io do newIORef True
    anonymousMode <- effIO io do newIORef True
    let store =
            UnsafeMkStore
                { gameFrame = gameFrame
                , leaderboardFrame = leaderboardFrame
                , snekDirection = snekDirection
                , sneks = sneks
                , maxFood = maxFood
                , foodPositions = foodPositions
                , newPlayer = newPlayer
                , maxPlayers = maxPlayers
                , isQueueFull = isQueueFull
                , queueMaxSize = queueMaxSize
                , boardSize = boardSize
                , gameFrameTimeMs = gameFrameTimeMs
                , renderWebComponent = renderWebComponent
                , chatContent = chatContent
                , chatContentHtml = chatContentHtml
                , chatMessages = chatMessages
                , disableChat = disableChat
                , anonymousMode = anonymousMode
                }
    let storeWrite = UnsafeMkStoreWrite (mapHandle io) (mapHandle stme) store
    let storeRead = UnsafeMkStoreRead (mapHandle io) (mapHandle stme) store
    useImplIn2 action storeRead storeWrite

getGameFrame :: (e :> es) => StoreRead e -> Eff es RawEvent
getGameFrame (UnsafeMkStoreRead io stme store) =
    effIO io $ readIORef store.gameFrame

putGameFrame :: (e :> es) => StoreWrite e -> RawEvent -> Eff es ()
putGameFrame (UnsafeMkStoreWrite io stme store) =
    effIO io . writeIORef store.gameFrame

getLeaderboardFrame :: (e :> es) => StoreRead e -> Eff es RawEvent
getLeaderboardFrame (UnsafeMkStoreRead io stme store) =
    effIO io $ readIORef store.leaderboardFrame

putLeaderboardFrame :: (e :> es) => StoreWrite e -> RawEvent -> Eff es ()
putLeaderboardFrame (UnsafeMkStoreWrite io stme store) = do
    effIO io . writeIORef store.leaderboardFrame

atomicModifySnekDirection
    :: (e :> es)
    => StoreWrite e
    -> (SneksDirections -> Eff es (SneksDirections, a))
    -> Eff es (SneksDirections, a)
atomicModifySnekDirection (UnsafeMkStoreWrite io stme store) f = do
    sneksDirection <- effIO io $ takeMVar store.snekDirection
    (newSneksDirections, res) <- f sneksDirection
    effIO io $ putMVar store.snekDirection newSneksDirections
    pure (newSneksDirections, res)

getSneks :: (e :> es) => StoreRead e -> Eff es Sneks
getSneks (UnsafeMkStoreRead io stme store) =
    effIO io $ readIORef store.sneks

putSneks :: (e :> es) => StoreWrite e -> Sneks -> Eff es ()
putSneks (UnsafeMkStoreWrite io stme store) =
    effIO io . writeIORef store.sneks

getMaxFood :: (e :> es) => StoreRead e -> Eff es Int
getMaxFood (UnsafeMkStoreRead io stme store) =
    effIO io $ readIORef store.maxFood

putMaxFood :: (e :> es) => StoreWrite e -> Int -> Eff es ()
putMaxFood (UnsafeMkStoreWrite io stme store) =
    effIO io . writeIORef store.maxFood

getFoodPositions :: (e :> es) => StoreRead e -> Eff es (Set (Int, Int))
getFoodPositions (UnsafeMkStoreRead io stme store) =
    effIO io $ readIORef store.foodPositions

putFoodPositions :: (e :> es) => StoreWrite e -> Set (Int, Int) -> Eff es ()
putFoodPositions (UnsafeMkStoreWrite io stme store) =
    effIO io . writeIORef store.foodPositions

getMaxPlayers :: (e :> es) => StoreRead e -> Eff es Int
getMaxPlayers (UnsafeMkStoreRead io stme store) =
    effIO io $ readIORef store.maxPlayers

putMaxPlayers :: (e :> es) => StoreWrite e -> Int -> Eff es ()
putMaxPlayers (UnsafeMkStoreWrite io stme store) =
    effIO io . writeIORef store.maxPlayers

getNewPlayer :: (e :> es) => StoreRead e -> Eff es (Maybe User)
getNewPlayer (UnsafeMkStoreRead io stme store) =
    BC.atomicallySTM stme do readTVar store.newPlayer

putNewPlayer :: (e :> es) => StoreWrite e -> Maybe User -> Eff es ()
putNewPlayer (UnsafeMkStoreWrite io stme store) user =
    BC.atomicallySTM stme do writeTVar store.newPlayer user

getIsQueueFull :: (e :> es) => StoreRead e -> Eff es Bool
getIsQueueFull (UnsafeMkStoreRead io stme store) =
    effIO io $ readIORef store.isQueueFull

putIsQueueFull :: (e :> es) => StoreWrite e -> Bool -> Eff es ()
putIsQueueFull (UnsafeMkStoreWrite io stme store) =
    effIO io . writeIORef store.isQueueFull

getQueueMaxSize :: (e :> es) => StoreRead e -> Eff es Natural
getQueueMaxSize (UnsafeMkStoreRead io stme store) =
    effIO io $ readIORef store.queueMaxSize

putQueueMaxSize :: (e :> es) => StoreWrite e -> Natural -> Eff es ()
putQueueMaxSize (UnsafeMkStoreWrite io stme store) =
    effIO io . writeIORef store.queueMaxSize

getBoardSize :: (e :> es) => StoreRead e -> Eff es Int
getBoardSize (UnsafeMkStoreRead io stme store) =
    effIO io $ readIORef store.boardSize

putBoardSize :: (e :> es) => StoreWrite e -> Int -> Eff es ()
putBoardSize (UnsafeMkStoreWrite io stme store) =
    effIO io . writeIORef store.boardSize

getGameFrameTimeMs :: (e :> es) => StoreRead e -> Eff es Integer
getGameFrameTimeMs (UnsafeMkStoreRead io stme store) =
    effIO io $ readIORef store.gameFrameTimeMs

putGameFrameTimeMs :: (e :> es) => StoreWrite e -> Integer -> Eff es ()
putGameFrameTimeMs (UnsafeMkStoreWrite io stme store) =
    effIO io . writeIORef store.gameFrameTimeMs

getRenderWebComponent :: (e :> es) => StoreRead e -> Eff es Bool
getRenderWebComponent (UnsafeMkStoreRead io stme store) =
    effIO io $ readIORef store.renderWebComponent

putRenderWebComponent :: (e :> es) => StoreWrite e -> Bool -> Eff es ()
putRenderWebComponent (UnsafeMkStoreWrite io stme store) =
    effIO io . writeIORef store.renderWebComponent

getChatMessages :: (e :> es) => StoreRead e -> Eff es [(User, Message)]
getChatMessages (UnsafeMkStoreRead io stme store) =
    effIO io $ readIORef store.chatMessages

putChatMessages :: (e :> es) => StoreWrite e -> [(User, Message)] -> Eff es ()
putChatMessages (UnsafeMkStoreWrite io stme store) =
    effIO io . writeIORef store.chatMessages

getChatContent :: (e :> es) => StoreRead e -> Eff es RawEvent
getChatContent (UnsafeMkStoreRead io stme store) =
    effIO io $ readIORef store.chatContent

putChatContent :: (e :> es) => StoreWrite e -> RawEvent -> Eff es ()
putChatContent (UnsafeMkStoreWrite io stme store) =
    effIO io . writeIORef store.chatContent

getChatContentHtml :: (e :> es) => StoreRead e -> Eff es (Html ())
getChatContentHtml (UnsafeMkStoreRead io stme store) =
    effIO io $ readIORef store.chatContentHtml

putChatContentHtml :: (e :> es) => StoreWrite e -> Html () -> Eff es ()
putChatContentHtml (UnsafeMkStoreWrite io stme store) =
    effIO io . writeIORef store.chatContentHtml

getDisableChat :: (e :> es) => StoreRead e -> Eff es Bool
getDisableChat (UnsafeMkStoreRead io stme store) =
    effIO io $ readIORef store.disableChat

putDisableChat :: (e :> es) => StoreWrite e -> Bool -> Eff es ()
putDisableChat (UnsafeMkStoreWrite io stme store) =
    effIO io . writeIORef store.disableChat

getAnonymousMode :: (e :> es) => StoreRead e -> Eff es Bool
getAnonymousMode (UnsafeMkStoreRead io stme store) =
    effIO io $ readIORef store.anonymousMode

putAnonymousMode :: (e :> es) => StoreWrite e -> Bool -> Eff es ()
putAnonymousMode (UnsafeMkStoreWrite io stme store) =
    effIO io . writeIORef store.anonymousMode
