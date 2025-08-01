{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Api where

import Bluefin.Coroutine (forEach)
import Bluefin.Eff (Eff, (:>))
import Bluefin.IO (IOE)
import Bluefin.Internal qualified
import Bluefin.Once (Once, only)
import Bluefin.Servant (singleToSourceIO, streamToSourceIO)
import Bluefin.State (evalState, get, put)
import Bluefin.Stream (yield)
import Broadcast (BroadcastClient, BroadcastServer, likeAndSubscribe, writeBroadcast)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Css (CSS)
import Data.Aeson ((.=))
import Data.Aeson qualified as Json
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Strict.Map qualified as Map
import Data.Text (Text)
import Data.UUID qualified as UUID
import Datastar qualified
import Favicon (Png)
import GHC.Generics (Generic)
import GenUuid (GenUuid, nextUuid)
import Lucid.Html5 (id_)
import Queue (Queue, tryWriteQueue)
import RawSse (RawEvent (..), SseGet, SsePost, ToSse (..))
import Servant (
    BasicAuth,
    BasicAuthData (..),
    BasicAuthResult (..),
    Capture,
    Context (..),
    GenericMode (type (:-)),
    Get,
    Handler,
    Headers,
    Proxy,
    QueryParam,
    ReqBody,
    SourceIO,
    addHeader,
    noHeader,
 )
import Servant qualified
import Servant.API (Header, JSON)
import Servant.Server (BasicAuthCheck (..))
import Store (
    StoreChatRead,
    StoreRead,
    StoreWrite,
    getChatDisabled,
    getChatEnabled,
    getIsQueueFull,
    getLoginPage,
    getMainPage,
    getSettings,
    maybeChatEnabled,
    putSettings,
 )
import Types (
    ChatMode (ChatCommands, ChatOff, ChatOn),
    Command (..),
    Direction,
    HtmlRaw,
    Message,
    RenderedHtml,
    Settings (chatMode),
    StoreUpdate (..),
    User (name, userId),
 )
import WebComponents (
    JS,
    location_,
    reload_,
    windowController_,
 )

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
    deriving stock (Show)

instance ToSse HotReload where
    toSse _ =
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
    deriving stock (Generic)

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
    :: (e1 :> es, e2 :> es, e3 :> es, e4 :> es)
    => StoreWrite e1
    -> StoreRead e2
    -> BroadcastServer StoreUpdate e3
    -> Queue () e4
    -> ()
    -> Settings
    -> Eff es (SourceIO EmptyResponse)
settings storeWrite storeRead broadcast mainPageQueue _ newSettings = do
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
nt :: forall es a e. IOE e -> Proxy es -> Eff es a -> Handler a
nt _ _ = liftIO . Bluefin.Internal.unsafeUnEff

ctx :: ByteString -> Context (BasicAuthCheck () ': '[])
ctx serverPassword = authCheck serverPassword :. EmptyContext

authCheck :: ByteString -> BasicAuthCheck ()
authCheck serverPassword = BasicAuthCheck $ \(BasicAuthData username password) -> do
    if username == "admin" && password == serverPassword
        then return (Authorized ())
        else return Unauthorized
