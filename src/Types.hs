{-# LANGUAGE DerivingVia #-}

module Types where

import Data.Aeson (FromJSON (parseJSON), withObject, withText, (.:))
import Data.Aeson qualified as Json
import Data.ByteString.Lazy qualified as BL
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import GHC.Natural (Natural)
import Lucid (Html, renderBS)
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept (contentTypes), MimeRender (..))
import Servant.HtmlRaw (HtmlRaw)

data Env = Prod | Dev
    deriving (Eq)

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

data ChatMode
    = ChatOn
    | ChatOff
    | ChatCommands
    deriving (Eq)

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
