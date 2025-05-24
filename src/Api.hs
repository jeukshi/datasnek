{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Api (run) where

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
import Command (Command (..), Direction (..))
import CommandManager qualified
import Control.Concurrent (putMVar, threadDelay)
import Control.Concurrent qualified
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar)
import Control.Concurrent.STM qualified as STM
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Css (CSS)
import Css qualified
import Data.Aeson (FromJSON (parseJSON), ToJSON, withObject, (.:))
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
import Favicon (Png)
import Favicon qualified
import GHC.Generics (Generic)
import Game qualified
import GenUuid (GenUuid, nextUuid, runGenUuid)
import Html
import JavaScript qualified
import Lucid (Html, HtmlT, ToHtml (..))
import Lucid.Base (makeAttributes, renderBS)
import Lucid.Datastar
import Lucid.Html5
import MainPageManager qualified
import Message
import Network.Wai.Handler.Warp qualified as Warp
import Numeric.Natural (Natural)
import Queue
import QueueManager qualified
import Random (runRandom)
import RawSse
import RenderHtml qualified
import Servant (
    Application,
    Capture,
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
 )
import Servant qualified
import Servant.API (Header, JSON, StdMethod (..), UVerb, WithStatus)
import Servant.API.ContentTypes.Lucid (HTML)
import Servant.API.Stream (SourceToSourceIO (..))
import Servant.Server.Generic (AsServerT, genericServeT)
import Servant.Types.SourceT (SourceT)
import Servant.Types.SourceT qualified as S
import Sleep
import Snek
import SnekWebComponent (JS, boardSize_, food_, snekGameBoard_, sneks_)
import SnekWebComponent qualified
import Store
import StoreUpdate
import Unsafe.Coerce (unsafeCoerce)
import User
import Web.FormUrlEncoded (FromForm (..), parseUnique)

type (:>>) = (Servant.:>)

infixr 4 :>>

data EmptyResponse = EmptyResponse

instance ToSse EmptyResponse where
    toSse _ = toSse (MkRawEvent "")

data RemoveComment = MkRemoveComment

instance ToSse RemoveComment where
    toSse _ = toSse do
        MkRawEvent
            ( "event:datastar-merge-signals\n"
                <> "data:signals {comment: ''}\n"
            )

data NewSettings = MkNewSettings
    { maxFood :: Int
    , maxPlayers :: Int
    , queueMaxSize :: Natural
    , boardSize :: Int
    , gameFrameTimeMs :: Integer
    , useWebComponent :: Bool
    , anonymousMode :: Bool
    , disableChat :: Bool
    , maxBots :: Int
    }

instance FromJSON NewSettings where
    parseJSON = withObject "Comment" $ \v ->
        MkNewSettings
            <$> v .: "maxFood"
            <*> v .: "maxPlayers"
            <*> v .: "queueMaxSize"
            <*> v .: "boardSize"
            <*> v .: "gameFrameTimeMs"
            <*> v .: "useWebComponent"
            <*> v .: "anonymousMode"
            <*> v .: "disableChat"
            <*> v .: "maxBots"

data HotReload = HotReload
    deriving (Show)

instance ToSse HotReload where
    toSse book = toSse do
        MkRawEvent
            ( "event:datastar-execute-script\n"
                <> "data:script "
                <> JavaScript.windowLocationReload
                <> "\n"
            )

data Transmittal
    = TransmittalRaw RawEvent
    | UpdateUsernameSignal Text
    | UpdateIsPlayingSignal Bool
    | LoginRedirect

instance ToSse Transmittal where
    toSse = \case
        (TransmittalRaw rawEvent) -> toSse rawEvent
        LoginRedirect -> toSse do
            MkRawEvent
                ( "event:datastar-execute-script\n"
                    <> "data:script "
                    <> JavaScript.windowLocationLogin
                    <> "\n"
                )
        UpdateUsernameSignal username -> toSse do
            MkRawEvent
                ( "event:datastar-merge-signals\n"
                    <> "data:signals {username: '"
                    <> renderBS (toHtml username)
                    <> "'}\n"
                )
        UpdateIsPlayingSignal isPlaying -> toSse do
            MkRawEvent
                ( "event:datastar-merge-signals\n"
                    <> "data:signals {isplaying: '"
                    <> (if isPlaying then "true" else "false")
                    <> "'}\n"
                )

data RedirectMain = MkRedirectMain

instance ToSse RedirectMain where
    toSse MkRedirectMain = toSse do
        MkRawEvent
            ( "event:datastar-execute-script\n"
                <> "data:script "
                <> JavaScript.windowLocationMain
                <> "\n"
            )

data Routes es route = Routes
    { _page
        :: route
            :- Get '[HtmlRaw] BL.ByteString
    , _loginPage :: route :- "login" :>> Get '[HTML] (Html ())
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
                :>> SsePost (SourceIO EmptyResponse)
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
                :>> SseGet (SourceIO EmptyResponse)
    , _settings
        :: -- TODO auth
        route
            :- "api"
                :>> "settings"
                :>> Header "Cookie" User
                :>> ReqBody '[JSON] NewSettings
                :>> SsePost (SourceIO EmptyResponse)
    , _css :: route :- "snek.css" :>> Get '[CSS] BS.ByteString
    , _snekcomponent :: route :- "snek-game-component.js" :>> Get '[JS] BS.ByteString
    , _favicon :: route :- "favicon.ico" :>> Get '[Png] BS.ByteString
    , _hotreload :: route :- "dev" :>> "hotreload" :>> SseGet (SourceIO HotReload)
    }
    deriving (Generic)

loginPage :: Eff es (Html ())
loginPage = return do
    pageHead do
        main_ [class_ "container"] do
            div_ [id_ "screen"] do
                form_ [dataOnSubmit_ JavaScript.login] do
                    input_ [name_ "username", placeholder_ "Username", required_ ""]
                    button_ "join"

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
    :: (e :> es) => BroadcastServer Command e -> Maybe User -> Eff es (SourceIO EmptyResponse)
play broadcast = \case
    Just user -> do
        -- FIXME check if is playing
        writeBroadcast broadcast (PlayRequest user)
        singleToSourceIO EmptyResponse
    Nothing -> do
        singleToSourceIO EmptyResponse

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
    -> Maybe User
    -> NewSettings
    -> Eff es (SourceIO EmptyResponse)
settings storeWrite storeRead broadcast chatQueue mainPageQueue = \cases
    -- FIXME auth
    (Just user) newSettings -> do
        putMaxFood storeWrite newSettings.maxFood
        putBoardSize storeWrite newSettings.boardSize
        putGameFrameTimeMs storeWrite newSettings.gameFrameTimeMs
        putMaxPlayers storeWrite newSettings.maxPlayers
        putQueueMaxSize storeWrite newSettings.queueMaxSize
        putRenderWebComponent storeWrite newSettings.useWebComponent
        putAnonymousMode storeWrite newSettings.anonymousMode
        putMaxBots storeWrite newSettings.maxBots
        settingsHtml <-
            RenderHtml.settings
                [ ("Max Food:", T.pack . show $ newSettings.maxFood)
                , ("Max Players:", T.pack . show $ newSettings.maxPlayers)
                , ("Queue Size:", T.pack . show $ newSettings.queueMaxSize)
                , ("Board Size:", T.pack . show $ newSettings.boardSize)
                ]
        putSettingsHtml storeWrite settingsHtml
        settingsFrame <- settingsToRawEvent settingsHtml
        writeBroadcast broadcast (SettingsFrameUpdate settingsFrame)
        oldDisableChat <- getDisableChat storeRead
        when (oldDisableChat /= newSettings.disableChat) do
            let disableChatRawEvent = disableChatToRawEvent newSettings.disableChat
            putDisableChat storeWrite newSettings.disableChat
            writeBroadcast broadcast (ChatInputUpdate disableChatRawEvent)
            -- This is a bit hacky. But we need to get chat manager to render the chat.
            writeQueue chatQueue (MkUser "" "", MkMessage "settings updated")
        _ <- tryWriteQueue mainPageQueue ()
        singleToSourceIO EmptyResponse
    Nothing _ -> do
        singleToSourceIO EmptyResponse
  where
    disableChatToRawEvent disabled =
        MkRawEvent
            ( "event:datastar-merge-signals\n"
                <> "data:signals {showchat: "
                <> (if disabled then "false" else "true")
                <> "}\n"
            )
    settingsToRawEvent :: Html () -> Eff es RawEvent
    settingsToRawEvent html = do
        pure $
            MkRawEvent $
                "event:datastar-merge-fragments\n"
                    <> "data:fragments "
                    <> renderBS html
                    <> "\n"

changeDirection
    :: (e :> es) => BroadcastServer Command e -> Direction -> Maybe User -> Eff es (SourceIO EmptyResponse)
changeDirection broadcast direction = \case
    Just user -> do
        -- FIXME check if is playing
        writeBroadcast broadcast (ChangeDirection user.userId direction)
        singleToSourceIO EmptyResponse
    Nothing -> do
        singleToSourceIO EmptyResponse

-- Stare at the transmittal. Sing to the snek rattle.
transmittal
    :: (e :> es)
    => BroadcastClient StoreUpdate e
    -> Maybe User
    -> Eff es (SourceIO Transmittal)
transmittal gameStateBroadcast = \case
    Just user -> streamToSourceIO \out -> do
        evalState False \isPlayingS -> do
            yield out $ UpdateUsernameSignal user.name
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
                ChatInputUpdate frame ->
                    yield out $ TransmittalRaw frame
                SettingsFrameUpdate frame ->
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

page :: (e :> es) => StoreRead e -> Eff es BL.ByteString
page = getMainPageBS

-- FIXME This foe is beyond me.
-- This code "works", but I can pass any handler to servant,
-- that then will be used concurrently!
nt :: forall es a e. (e :> es) => IOE e -> Proxy es -> Eff es a -> Handler a
nt _ _ = liftIO . Bluefin.Internal.unsafeUnEff

run :: IO ()
run = runEff \io -> do
    -- Bluefin pyramid of doom.
    BC.runSTM io \stme -> do
        runStore io stme \storeReadMain storeWriteMain -> do
            runBroadcast io storeReadMain \broadcastGameStateClientMain broadcastGameStateServerMain -> do
                runBroadcast io storeReadMain \broadcastCommandClientMain broadcastCommandServerMain -> do
                    runRandom io \randomMain -> do
                        runGenUuid io \genUuidMain -> do
                            runQueue stme 100 \gameQueueMain -> do
                                runQueue stme 100 \chatQueueMain -> do
                                    runQueue stme 2 \mainPageQueueMain -> do
                                        BC.withNonDet io \nonDet -> do
                                            BC.scoped nonDet \scope -> do
                                                _ <- BC.fork scope $ \acc -> do
                                                    broadcastGameStateServer <- accessConcurrently acc broadcastGameStateServerMain
                                                    broadcastGameStateClient <- accessConcurrently acc broadcastGameStateClientMain
                                                    storeRead <- accessConcurrently acc storeReadMain
                                                    storeWrite <- accessConcurrently acc storeWriteMain
                                                    storeWrite <- accessConcurrently acc storeWriteMain
                                                    mainPageQueue <- accessConcurrently acc mainPageQueueMain
                                                    MainPageManager.run storeWrite storeRead mainPageQueue
                                                    pure ()

                                                _ <- BC.fork scope $ \acc -> do
                                                    random <- accessConcurrently acc randomMain
                                                    broadcastGameStateServer <- accessConcurrently acc broadcastGameStateServerMain
                                                    broadcastCommandClient <- accessConcurrently acc broadcastCommandClientMain
                                                    storeRead <- accessConcurrently acc storeReadMain
                                                    storeWrite <- accessConcurrently acc storeWriteMain
                                                    chatQueue <- accessConcurrently acc chatQueueMain
                                                    mainPageQueue <- accessConcurrently acc mainPageQueueMain
                                                    ChatManager.run storeWrite storeRead chatQueue broadcastGameStateServer mainPageQueue

                                                _ <- BC.fork scope $ \acc -> do
                                                    random <- accessConcurrently acc randomMain
                                                    broadcastGameStateClient <- accessConcurrently acc broadcastGameStateClientMain
                                                    broadcastCommandServer <- accessConcurrently acc broadcastCommandServerMain
                                                    storeRead <- accessConcurrently acc storeReadMain
                                                    storeWrite <- accessConcurrently acc storeWriteMain
                                                    chatQueue <- accessConcurrently acc chatQueueMain
                                                    genUuid <- accessConcurrently acc genUuidMain
                                                    BotManager.run storeRead broadcastGameStateClient broadcastCommandServer random genUuid

                                                _ <- BC.fork scope $ \acc -> do
                                                    random <- accessConcurrently acc randomMain
                                                    broadcastGameStateServer <- accessConcurrently acc broadcastGameStateServerMain
                                                    broadcastCommandClient <- accessConcurrently acc broadcastCommandClientMain
                                                    storeRead <- accessConcurrently acc storeReadMain
                                                    storeWrite <- accessConcurrently acc storeWriteMain
                                                    gameQueue <- accessConcurrently acc gameQueueMain
                                                    localIO <- accessConcurrently acc io
                                                    runSleep localIO \sleep -> do
                                                        QueueManager.run storeWrite storeRead gameQueue broadcastGameStateServer sleep

                                                _ <- BC.fork scope $ \acc -> do
                                                    random <- accessConcurrently acc randomMain
                                                    broadcastGameStateServer <- accessConcurrently acc broadcastGameStateServerMain
                                                    broadcastCommandClient <- accessConcurrently acc broadcastCommandClientMain
                                                    storeRead <- accessConcurrently acc storeReadMain
                                                    storeWrite <- accessConcurrently acc storeWriteMain
                                                    gameQueue <- accessConcurrently acc gameQueueMain
                                                    chatQueue <- accessConcurrently acc chatQueueMain
                                                    localScope <- accessConcurrently acc scope
                                                    growScope localScope \newScope -> do
                                                        CommandManager.run
                                                            random
                                                            storeWrite
                                                            storeRead
                                                            gameQueue
                                                            chatQueue
                                                            newScope
                                                            broadcastCommandClient
                                                            broadcastGameStateServer

                                                _ <- BC.fork scope $ \acc -> do
                                                    random <- accessConcurrently acc randomMain
                                                    broadcastGameStateServer <- accessConcurrently acc broadcastGameStateServerMain
                                                    broadcastCommandClient <- accessConcurrently acc broadcastCommandClientMain
                                                    storeRead <- accessConcurrently acc storeReadMain
                                                    storeWrite <- accessConcurrently acc storeWriteMain
                                                    gameQueue <- accessConcurrently acc gameQueueMain
                                                    chatQueue <- accessConcurrently acc chatQueueMain
                                                    localScope <- accessConcurrently acc scope
                                                    mainPageQueue <- accessConcurrently acc mainPageQueueMain
                                                    localIO <- accessConcurrently acc io
                                                    runSleep localIO \sleep -> do
                                                        growScope localScope \newScope -> do
                                                            Game.run
                                                                random
                                                                storeWrite
                                                                storeRead
                                                                gameQueue
                                                                chatQueue
                                                                newScope
                                                                broadcastCommandClient
                                                                broadcastGameStateServer
                                                                mainPageQueue
                                                                sleep

                                                warpThread <- BC.fork scope $ \acc -> do
                                                    ioLocal <- accessConcurrently acc io
                                                    broadcastGameStateClient <- accessConcurrently acc broadcastGameStateClientMain
                                                    broadcastCommandServer <- accessConcurrently acc broadcastCommandServerMain
                                                    chatQueue <- accessConcurrently acc chatQueueMain
                                                    genUuid <- accessConcurrently acc genUuidMain
                                                    storeRead <- accessConcurrently acc storeReadMain
                                                    storeWrite <- accessConcurrently acc storeWriteMain
                                                    broadcastGameStateServer <- accessConcurrently acc broadcastGameStateServerMain
                                                    mainPageQueue <- accessConcurrently acc mainPageQueueMain
                                                    runOnce ioLocal \once -> do
                                                        effEnvProxy <- captureEffEnv
                                                        let record =
                                                                Routes
                                                                    { _page = page storeRead
                                                                    , _loginPage = loginPage
                                                                    , _transmittal = transmittal broadcastGameStateClient
                                                                    , _hotreload = hotreload once
                                                                    , _play = play broadcastCommandServer
                                                                    , _chat = chat broadcastCommandServer
                                                                    , _changeDirection = changeDirection broadcastCommandServer
                                                                    , _login = login genUuid
                                                                    , _css = pure Css.file
                                                                    , _favicon = pure Favicon.file
                                                                    , _snekcomponent = pure SnekWebComponent.file
                                                                    , _settings =
                                                                        settings
                                                                            storeWrite
                                                                            storeRead
                                                                            broadcastGameStateServer
                                                                            chatQueue
                                                                            mainPageQueue
                                                                    }
                                                        let app :: Application = genericServeT (nt ioLocal effEnvProxy) record
                                                        effIO ioLocal do Warp.run 3000 app
                                                BC.awaitEff warpThread

pageHead :: (Monad m) => HtmlT m a -> HtmlT m a
pageHead main = doctypehtml_ do
    head_ do
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        meta_ [name_ "color-scheme", content_ "light dark"]
        link_ [rel_ "icon", type_ "image/png", href_ "/favicon.ico"]
        link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css"]
        link_ [rel_ "stylesheet", href_ "snek.css"]
        link_
            [ rel_ "stylesheet"
            , href_
                "https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@40,400,0,0&icon_names=cancel"
            ]
        script_
            [ type_
                "module"
            , src_
                "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.0-beta.11/bundles/datastar.js"
            ]
            ("" :: Text)
        script_ [src_ "snek-game-component.js"] ("" :: Text)
        title_ "Datasnek"
    body_ do
        main
