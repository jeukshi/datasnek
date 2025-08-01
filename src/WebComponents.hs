{-# LANGUAGE TemplateHaskell #-}

module WebComponents where

import Data.ByteString qualified
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Lucid
import Lucid.Base
import Network.HTTP.Media qualified as M
import Servant.API (Accept, MimeRender (..))
import Servant.API.ContentTypes (Accept (..))

snekGameControllerJS :: Data.ByteString.ByteString
snekGameControllerJS = $(embedFile "static/snek-game-component.js")

data JS deriving stock (Typeable)

instance Accept JS where
    contentType _ = "application" M.// "javascript"

instance MimeRender JS BS.ByteString where
    mimeRender _ = BL.fromStrict

snekGameBoard_ :: (Monad m) => [Attributes] -> HtmlT m a -> HtmlT m a
snekGameBoard_ = makeElement "snek-game-board"

boardSize_ :: Text -> Attributes
boardSize_ = makeAttributes "board-size"

food_ :: Text -> Attributes
food_ = makeAttributes "food"

sneks_ :: Text -> Attributes
sneks_ = makeAttributes "sneks"

username_ :: Text -> Attributes
username_ = makeAttributes "username"

anonymous_ :: Bool -> Attributes
anonymous_ = \cases
    True -> makeAttributes "anonymous" "true"
    False -> makeAttributes "anonymous" "false"

windowControllerJS :: Data.ByteString.ByteString
windowControllerJS = $(embedFile "static/window-controller.js")

windowController_ :: (Monad m) => [Attributes] -> HtmlT m a -> HtmlT m a
windowController_ = makeElement "window-controller"

location_ :: Text -> Attributes
location_ = makeAttributes "location"

reload_ :: Attributes
reload_ = makeAttributes "reload" ""
