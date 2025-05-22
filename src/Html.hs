module Html where

import Data.ByteString.Lazy qualified as BL
import Data.Typeable (Typeable)
import Network.HTTP.Media qualified as M
import Servant.API (Accept (..), MimeRender (..))

data HtmlRaw deriving (Typeable)

instance Accept HtmlRaw where
    contentType _ = "text" M.// "html"

instance MimeRender HtmlRaw BL.ByteString where
    mimeRender _ = id
