{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Api where

import Bluefin.Compound (useImplUnder)
import Bluefin.Concurrent.Local qualified as BC
import Bluefin.Consume (await)
import Bluefin.Coroutine (Coroutine, connectCoroutines)
import Bluefin.EarlyReturn
import Bluefin.Eff (Eff, bracket, runEff, (:&), (:>))
import Bluefin.Exception (throw, try)
import Bluefin.Extra (accessConcurrently, growScope)
import Bluefin.IO (IOE, effIO)
import Bluefin.Internal qualified
import Bluefin.Once
import Bluefin.Proxy (captureEffEnv)
import Bluefin.Reader
import Bluefin.Servant (singleToSourceIO, streamToSourceIO)
import Bluefin.State
import Bluefin.Stream (Stream, consumeStream, forEach, yield)
import BotManager qualified
import Broadcast
import ChatManager qualified
import Color (generateColors)
import CommandManager qualified
import Control.Concurrent (putMVar, threadDelay)
import Control.Concurrent qualified
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar)
import Control.Concurrent.STM qualified as STM
import Control.Monad (forever, unless, when)
import Control.Monad.IO.Class (liftIO)
import Css (CSS)
import Css qualified
import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), ToJSON, withObject, (.:))
import Data.Aeson qualified as Json
import Data.Aeson.Types (FromJSON)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Foldable qualified as Foldable
import Data.Strict.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Data.UUID.V4 qualified as UUID
import Datastar qualified
import Favicon (Png)
import Favicon qualified
import GHC.Generics (Generic)
import Game qualified
import GenUuid (GenUuid, nextUuid, runGenUuid)
import Html qualified
import JavaScript qualified
import Lucid (Html, HtmlT, ToHtml (..))
import Lucid.Base (makeAttributes, renderBS)
import Lucid.Datastar
import Lucid.Html5
import Network.Wai.Handler.Warp qualified as Warp
import Numeric.Natural (Natural)
import Queue
import QueueManager qualified
import Random (runRandom)
import RawSse
import Servant (
    Application,
    BasicAuth,
    BasicAuthData (..),
    BasicAuthResult (..),
    Capture,
    Context (..),
    Delete,
    FormUrlEncoded,
    GenericMode (type (:-)),
    Get,
    Handler,
    Headers,
    OctetStream,
    Post,
    Proxy,
    QueryParam,
    ReqBody,
    SourceIO,
    addHeader,
    noHeader,
    serveWithContext,
    serveWithContextT,
 )
import Servant qualified
import Servant.API (Header, JSON, StdMethod (..), UVerb, WithStatus)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.API.Stream (SourceToSourceIO (..))
import Servant.HtmlRaw
import Servant.Server (BasicAuthCheck (..))
import Servant.Server.Generic (AsServerT, genericServeT, genericServeTWithContext)
import Servant.Types.SourceT (SourceT)
import Servant.Types.SourceT qualified as S
import Sleep
import Store
import Types
import Unsafe.Coerce (unsafeCoerce)
import Web.FormUrlEncoded (FromForm (..), parseUnique)
import WebComponents (
    JS,
    boardSize_,
    food_,
    location_,
    reload_,
    snekGameBoard_,
    sneks_,
    windowController_,
 )
import WebComponents qualified

type (:>>) = (Servant.:>)

infixr 4 :>>

data EmptyResponse = MkEmptyResponse

instance ToSse EmptyResponse where
    toSse _ = toSse (MkRawEvent "")

data RemoveComment = MkRemoveComment

instance ToSse RemoveComment where
    toSse _ = toSse do
        Datastar.patchSignals $ Json.object ["comment" .= ("" :: Text)]

data SetInQueue = MkSetInQueue

instance ToSse SetInQueue where
    toSse _ = toSse do
        Datastar.patchSignals $ Json.object ["inqueue" .= True]

data HotReload = HotReload
    deriving (Show)

instance ToSse HotReload where
    toSse book =
        toSse . Datastar.patchElements $
            windowController_ [id_ "window-controller", reload_] mempty

data Transmittal
    = TransmittalRaw RawEvent
    | UpdateUsernameSignal Text
    | UpdateIsPlayingSignal Bool
    | LoginRedirect

instance ToSse Transmittal where
    toSse = \case
        (TransmittalRaw rawEvent) -> toSse rawEvent
        LoginRedirect ->
            toSse . Datastar.patchElements $
                windowController_ [id_ "window-controller", location_ "/login"] mempty
        UpdateUsernameSignal username -> toSse do
            Datastar.patchSignals $ Json.object ["username" .= username]
        UpdateIsPlayingSignal isPlaying -> toSse do
            Datastar.patchSignals $
                Json.object
                    ["isplaying" .= isPlaying, "inqueue" .= isPlaying]

data RedirectMain = MkRedirectMain

instance ToSse RedirectMain where
    toSse MkRedirectMain =
        toSse . Datastar.patchElements $
            windowController_ [id_ "window-controller", location_ "/"] mempty

data Routes es route = Routes
    { _page
        :: route
            :- Get '[HtmlRaw] RenderedHtml
    , _loginPage :: route :- "login" :>> Get '[HtmlRaw] RenderedHtml
    , _transmittal
        :: route
            :- "api"
                :>> "transmittal"
                :>> Header "Cookie" User
                :>> SseGet (SourceIO Transmittal)
    , _login
        :: route
            :- "api"
                :>> "login"
                :>> QueryParam "username" Text
                :>> SseGet
                        (Headers '[Header "Set-Cookie" Text, Header "Set-Cookie" Text] (SourceIO RedirectMain))
    , _play
        :: route
            :- "api"
                :>> "play"
                :>> Header "Cookie" User
                :>> SsePost (SourceIO SetInQueue)
    , _chat
        :: route
            :- "api"
                :>> "chat"
                :>> Header "Cookie" User
                :>> ReqBody '[JSON] Message
                :>> SsePost (SourceIO RemoveComment)
    , _changeDirection
        :: route
            :- "api"
                :>> "game"
                :>> "change-direction"
                :>> Capture "dir" Direction
                :>> Header "Cookie" User
                :>> SsePost (SourceIO EmptyResponse)
    , _settings
        :: -- TODO auth
        route
            :- "api"
                :>> "settings"
                :>> BasicAuth "you shall not pass" ()
                :>> ReqBody '[JSON] Settings
                :>> SsePost (SourceIO EmptyResponse)
    , _css :: route :- "snek.css" :>> Get '[CSS] BS.ByteString
    , _snekcomponent :: route :- "snek-game-component.js" :>> Get '[JS] BS.ByteString
    , _windowController :: route :- "window-controller.js" :>> Get '[JS] BS.ByteString
    , _favicon :: route :- "favicon.ico" :>> Get '[Png] BS.ByteString
    , _hotreload :: route :- "dev" :>> "hotreload" :>> SseGet (SourceIO HotReload)
    }
    deriving (Generic)

loginPage :: (e :> es) => StoreRead e -> Eff es RenderedHtml
loginPage = getLoginPage

login
    :: (e :> es)
    => GenUuid e
    -> Maybe Text
    -> Eff
        es
        (Headers '[Header "Set-Cookie" Text, Header "Set-Cookie" Text] (SourceIO RedirectMain))
login genUuid = \case
    Nothing -> do
        res <- singleToSourceIO MkRedirectMain
        return $ noHeader (noHeader res)
    Just name -> do
        res <- singleToSourceIO MkRedirectMain
        uuid <- UUID.toText <$> nextUuid genUuid
        let uuidCookie = uuid <> "; Path=/; Max-Age=86400"
        let nameCookie = name <> "; Path=/; Max-Age=86400"
        return $ addHeader ("datasnek-id=" <> uuidCookie) (addHeader ("datasnek-name=" <> nameCookie) res)

play
    :: (e1 :> es, e2 :> es)
    => StoreRead e1
    -> BroadcastServer Command e2
    -> Maybe User
    -> Eff es (SourceIO SetInQueue)
play storeRead broadcast = \case
    Just user -> do
        isQueueFull <- getIsQueueFull storeRead
        unless isQueueFull do
            writeBroadcast broadcast (PlayRequest user)
        singleToSourceIO MkSetInQueue
    Nothing -> do
        singleToSourceIO MkSetInQueue -- FIXME

chat
    :: (e :> es) => BroadcastServer Command e -> Maybe User -> Message -> Eff es (SourceIO RemoveComment)
chat broadcast = \cases
    (Just user) comment -> do
        writeBroadcast broadcast (NewComment user comment)
        singleToSourceIO MkRemoveComment
    Nothing _ -> do
        singleToSourceIO MkRemoveComment

settings
    :: (e1 :> es, e2 :> es, e3 :> es, e4 :> es, e5 :> es)
    => StoreWrite e1
    -> StoreRead e2
    -> BroadcastServer StoreUpdate e3
    -> Queue (User, Message) e4
    -> Queue () e5
    -> ()
    -> Settings
    -> Eff es (SourceIO EmptyResponse)
settings storeWrite storeRead broadcast chatQueue mainPageQueue _ newSettings = do
    oldChatMode <- (.chatMode) <$> getSettings storeRead
    putSettings storeWrite newSettings
    case (oldChatMode, newSettings.chatMode) of
        (ChatOn, ChatOff) -> chatOff
        (ChatCommands, ChatOff) -> chatOff
        (ChatOff, ChatOn) -> chatOn
        (ChatOff, ChatCommands) -> chatOn
        _ -> pure ()
    _ <- tryWriteQueue mainPageQueue ()
    singleToSourceIO MkEmptyResponse
  where
    chatOn = do
        enableChatEvent <- getChatEnabled storeRead
        writeBroadcast broadcast (ChatEnable enableChatEvent)
    chatOff = do
        disableChatEvent <- getChatDisabled storeRead
        writeBroadcast broadcast (ChatDisable disableChatEvent)

changeDirection
    :: (e :> es) => BroadcastServer Command e -> Direction -> Maybe User -> Eff es (SourceIO EmptyResponse)
changeDirection broadcast direction = \case
    Just user -> do
        -- FIXME check if is playing
        writeBroadcast broadcast (ChangeDirection user.userId direction)
        singleToSourceIO MkEmptyResponse
    Nothing -> do
        singleToSourceIO MkEmptyResponse

-- Stare at the transmittal. Sing to the snek rattle.
transmittal
    :: (e1 :> es, e2 :> es)
    => StoreChatRead e1
    -> BroadcastClient StoreUpdate e2
    -> Maybe User
    -> Eff es (SourceIO Transmittal)
transmittal store gameStateBroadcast = \case
    Just user -> streamToSourceIO \out -> do
        evalState False \isPlayingS -> do
            yield out $ UpdateUsernameSignal user.name
            maybeChatEnabled store >>= \case
                Nothing -> pure ()
                -- There is a race condition here. Before reading any state,
                -- we should first subscribe to events.
                Just chatEnabled -> yield out (TransmittalRaw chatEnabled)
            forEach (likeAndSubscribe gameStateBroadcast) \case
                GameFrameUpdate framePerUser defaultFrame sneksDirections -> do
                    let frame = Map.findWithDefault defaultFrame user.userId framePerUser
                    yield out $ TransmittalRaw frame
                    playingStateChange out isPlayingS user sneksDirections
                WebComponentUpdate frame sneksDirections -> do
                    yield out $ TransmittalRaw frame
                    playingStateChange out isPlayingS user sneksDirections
                ChatFrameUpdate frame ->
                    yield out $ TransmittalRaw frame
                ChatNewMessage message ->
                    yield out $ TransmittalRaw message
                ChatEnable frame ->
                    yield out $ TransmittalRaw frame
                ChatDisable frame ->
                    yield out $ TransmittalRaw frame
                UsernameUpdate event ->
                    yield out $ TransmittalRaw event
    Nothing -> singleToSourceIO LoginRedirect
  where
    playingStateChange out isPlayingS user sneksDirections = do
        isPlaying <- get isPlayingS
        case (Map.member user.userId sneksDirections, isPlaying) of
            (True, False) -> do
                put isPlayingS True
                yield out $ UpdateIsPlayingSignal True
            (False, True) -> do
                put isPlayingS False
                yield out $ UpdateIsPlayingSignal False
            _ -> pure ()

hotreload :: (e :> es) => Once e -> Eff es (SourceIO HotReload)
hotreload once = only once $ streamToSourceIO \out -> do
    yield out HotReload
    only once $ yield out HotReload

page :: (e :> es) => StoreRead e -> Eff es RenderedHtml
page = getMainPage

-- FIXME This foe is beyond me.
-- This code "works", but I can pass any handler to servant,
-- that then will be used concurrently!
nt :: forall es a e. (e :> es) => IOE e -> Proxy es -> Eff es a -> Handler a
nt _ _ = liftIO . Bluefin.Internal.unsafeUnEff

ctx :: ByteString -> Context (BasicAuthCheck () ': '[])
ctx serverPassword = authCheck serverPassword :. EmptyContext

authCheck :: ByteString -> BasicAuthCheck ()
authCheck serverPassword = BasicAuthCheck $ \(BasicAuthData username password) -> do
    if username == "admin" && password == serverPassword
        then return (Authorized ())
        else return Unauthorized
