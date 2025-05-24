{-# LANGUAGE DerivingVia #-}

module User where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Servant.API (FromHttpApiData (..))
import Web.Cookie (parseCookiesText)

newtype UserId = UnsafeMkUserId {fromUserId :: Text}
    deriving (Eq, Ord, IsString, Show) via Text

data User = MkUser
    { name :: Text
    , userId :: UserId
    }
    deriving (Eq, Show)

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
