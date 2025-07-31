{-# LANGUAGE DerivingVia #-}

module Types where

import Data.Aeson (FromJSON (parseJSON), withObject, withText, (.:))
import Data.ByteString.Lazy qualified as BL
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as NE
import Data.Strict.Map (Map)
import Data.Strict.Set (Set)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Lucid (Html, renderBS)
import Network.HTTP.Media ((//), (/:))
import RawSse (RawEvent)
import Servant (FromHttpApiData (..))
import Servant.API (Accept (..), MimeRender (..))
import Web.Cookie (parseCookiesText)

data Env = Prod | Dev
    deriving stock (Eq)

data HtmlRaw deriving stock (Typeable)

instance Accept HtmlRaw where
    contentType _ = "text" // "html"

instance MimeRender HtmlRaw BL.ByteString where
    mimeRender _ = id

newtype RenderedHtml where
    MkRenderedHtml :: {unRenderedHtml :: BL.ByteString} -> RenderedHtml

instance Accept RenderedHtml where
    contentTypes _ =
        "text" // "html" /: ("charset", "utf-8")
            NE.:| ["text" // "html"]

instance MimeRender HtmlRaw RenderedHtml where
    mimeRender _ = coerce

renderHtml :: Html () -> RenderedHtml
renderHtml = coerce . renderBS

newtype Message = MkMessage {text :: Text}
    deriving (Eq) via Text

instance FromJSON Message where
    parseJSON = withObject "Comment" $ \v ->
        MkMessage
            <$> v .: "comment"

data GameState = MkGameState
    { boardSize :: Int
    , foodPositions :: Set (Int, Int)
    , aliveSneks :: Sneks
    , newPlayer :: Maybe (Snek, SnekDirection)
    , maxPlayers :: Int
    , snekSelfOwn :: Bool
    }
    deriving stock (Show)

data Command
    = ChangeDirection UserId Direction
    | PlayRequest User
    | NewComment User Message
    deriving stock (Eq)

data Direction = U | D | L | R
    deriving stock (Eq, Show)

instance FromHttpApiData Direction where
    parseUrlPiece t = case T.toLower t of
        "u" -> Right U
        "d" -> Right D
        "l" -> Right L
        "r" -> Right R
        _ -> Left $ "Invalid direction: " <> t

newtype UserId = UnsafeMkUserId {fromUserId :: Text}
    deriving (Eq, Ord, IsString, Show) via Text

data User = MkUser
    { name :: Text
    , userId :: UserId
    }
    deriving stock (Eq, Show)

userIdToText :: UserId -> Text
userIdToText = coerce

userIdFromUuid :: UUID -> UserId
userIdFromUuid = coerce . UUID.toText

instance FromHttpApiData User where
    parseQueryParam = const $ Left "Not implemented"
    parseHeader header = do
        let cookies = parseCookiesText header
        name <- maybeToEither "Missing datasnek-name" $ lookup "datasnek-name" cookies
        uid <- maybeToEither "Missing datasnek-id" $ lookup "datasnek-id" cookies
        pure $ MkUser name (coerce uid)

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err = maybe (Left err) Right

type Sneks = [Snek]

data Snek = MkSnek
    { user :: User
    , color :: Text
    , headOfSnek :: (Int, Int)
    , restOfSnek :: [(Int, Int)]
    , grace :: Int
    }
    deriving stock (Show, Eq)

type SneksDirections = Map UserId SnekDirection

data SnekDirection = MkSnekDirection
    { current :: Direction
    , new :: Maybe Direction
    }
    deriving stock (Show, Generic)

data StoreUpdate
    = GameFrameUpdate (Map UserId RawEvent) RawEvent SneksDirections
    | WebComponentUpdate RawEvent SneksDirections
    | ChatFrameUpdate RawEvent
    | ChatNewMessage RawEvent
    | UsernameUpdate RawEvent
    | ChatEnable RawEvent
    | ChatDisable RawEvent

data ChatMode
    = ChatOn
    | ChatOff
    | ChatCommands
    deriving stock (Eq)

instance FromJSON ChatMode where
    parseJSON = withText "ChatMode" $ \t -> case T.toLower t of
        "on" -> pure ChatOn
        "off" -> pure ChatOff
        "commands" -> pure ChatCommands
        _ -> fail $ "Invalid ChatMode: " <> T.unpack t

data Settings = MkSettings
    { maxFood :: Int
    , maxPlayers :: Int
    , queueMaxSize :: Natural
    , boardSize :: Int
    , frameTimeMs :: Int
    , useWebComponent :: Bool
    , anonymousMode :: Bool
    , chatMode :: ChatMode
    , maxBots :: Int
    , gracePeriod :: Int
    , snekSelfOwn :: Bool
    , snekCanReverse :: Bool
    , chatCanChange :: ChatCanChange
    }

instance FromJSON Settings where
    parseJSON = withObject "Settings" $ \v -> do
        maxFood <- v .: "maxFood"
        maxPlayers <- v .: "maxPlayers"
        queueMaxSize <- v .: "queueMaxSize"
        boardSize <- v .: "boardSize"
        frameTimeMs <- v .: "frameTimeMs"
        useWebComponent <- v .: "useWebComponent"
        anonymousMode <- v .: "anonymousMode"
        chatMode <- v .: "chatMode"
        maxBots <- v .: "maxBots"
        gracePeriod <- v .: "gracePeriod"
        snekSelfOwn <- v .: "snekSelfOwn"
        snekCanReverse <- v .: "snekCanReverse"
        chatCanChange <- v .: "chatCanChange"
        pure
            MkSettings
                { maxFood = maxFood
                , maxPlayers = maxPlayers
                , queueMaxSize = queueMaxSize
                , boardSize = boardSize
                , frameTimeMs = frameTimeMs
                , useWebComponent = useWebComponent
                , anonymousMode = anonymousMode
                , chatMode = chatMode
                , maxBots = maxBots
                , gracePeriod = gracePeriod
                , snekSelfOwn = snekSelfOwn
                , snekCanReverse = snekCanReverse
                , chatCanChange = chatCanChange
                }

data ChatCanChange = MkChatCanChange
    { botsMin :: Int
    , botsMax :: Int
    , playersMin :: Int
    , playersMax :: Int
    , foodMin :: Int
    , foodMax :: Int
    , boardSizeMin :: Int
    , boardSizeMax :: Int
    , boardSizeWcMin :: Int
    , boardSizeWcMax :: Int
    , gracePeriodMin :: Int
    , gracePeriodMax :: Int
    , selfOwn :: Bool
    , canReverse :: Bool
    , webComponent :: Bool
    , frameTimeMsMin :: Int
    , frameTimeMsMax :: Int
    }

instance FromJSON ChatCanChange where
    parseJSON = withObject "ChatCanChange" $ \v -> do
        botsMin <- v .: "botsMin"
        botsMax <- v .: "botsMax"
        playersMin <- v .: "playersMin"
        playersMax <- v .: "playersMax"
        foodMin <- v .: "foodMin"
        foodMax <- v .: "foodMax"
        boardSizeMin <- v .: "boardSizeMin"
        boardSizeMax <- v .: "boardSizeMax"
        boardSizeWcMin <- v .: "boardSizeWcMin"
        boardSizeWcMax <- v .: "boardSizeWcMax"
        gracePeriodMin <- v .: "gracePeriodMin"
        gracePeriodMax <- v .: "gracePeriodMax"
        selfOwn <- v .: "selfOwn"
        canReverse <- v .: "canReverse"
        webComponent <- v .: "webComponent"
        frameTimeMsMin <- v .: "frameTimeMsMin"
        frameTimeMsMax <- v .: "frameTimeMsMax"
        pure
            MkChatCanChange
                { botsMin = botsMin
                , botsMax = botsMax
                , playersMin = playersMin
                , playersMax = playersMax
                , foodMin = foodMin
                , foodMax = foodMax
                , boardSizeMin = boardSizeMin
                , boardSizeMax = boardSizeMax
                , boardSizeWcMin = boardSizeWcMin
                , boardSizeWcMax = boardSizeWcMax
                , gracePeriodMin = gracePeriodMin
                , gracePeriodMax = gracePeriodMax
                , selfOwn = selfOwn
                , canReverse = canReverse
                , webComponent = webComponent
                , frameTimeMsMin = frameTimeMsMin
                , frameTimeMsMax = frameTimeMsMax
                }
