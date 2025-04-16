{-# LANGUAGE TemplateHaskell #-}

module Css where

import Data.ByteString qualified
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.FileEmbed (embedFile)
import Data.Typeable (Typeable)
import Network.HTTP.Media qualified as M
import Servant.API (Accept, MimeRender (..))
import Servant.API.ContentTypes (Accept (..))

file :: Data.ByteString.ByteString
file = $(embedFile "static/snek.css")

data CSS deriving (Typeable)

instance Accept CSS where
    contentType _ = "text" M.// "css"

instance MimeRender CSS BS.ByteString where
    mimeRender _ = BL.fromStrict
