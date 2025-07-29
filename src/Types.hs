{-# LANGUAGE DerivingVia #-}

module Types where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.ByteString.Lazy qualified as BL
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as NE
import GHC.Natural (Natural)
import Lucid (Html, renderBS)
import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept (contentTypes), MimeRender (..))
import Servant.HtmlRaw (HtmlRaw)

data Env = Prod | Dev

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

data Settings = MkSettings
    { maxFood :: Int
    , maxPlayers :: Int
    , queueMaxSize :: Natural
    , boardSize :: Int
    , gameFrameTimeMs :: Integer
    , useWebComponent :: Bool
    , anonymousMode :: Bool
    , disableChat :: Bool
    , maxBots :: Int
    , gracePeriod :: Int
    , snekSelfOwn :: Bool
    , snekCanReverse :: Bool
    }

instance FromJSON Settings where
    parseJSON = withObject "Comment" $ \v ->
        MkSettings
            <$> v .: "maxFood"
            <*> v .: "maxPlayers"
            <*> v .: "queueMaxSize"
            <*> v .: "boardSize"
            <*> v .: "gameFrameTimeMs"
            <*> v .: "useWebComponent"
            <*> v .: "anonymousMode"
            <*> v .: "disableChat"
            <*> v .: "maxBots"
            <*> v .: "gracePeriod"
            <*> v .: "snekSelfOwn"
            <*> v .: "snekCanReverse"
