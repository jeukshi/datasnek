{-# LANGUAGE TemplateHaskell #-}

module Favicon where

import Data.ByteString qualified
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.FileEmbed (embedFile)
import Data.Typeable (Typeable)
import Network.HTTP.Media qualified as M
import Servant.API (Accept, MimeRender (..))
import Servant.API.ContentTypes (Accept (..))

file :: Data.ByteString.ByteString
file = $(embedFile "static/favicon.png")

data Png deriving stock (Typeable)

instance Accept Png where
    contentType _ = "image" M.// "png"

instance MimeRender Png BS.ByteString where
    mimeRender _ = BL.fromStrict
