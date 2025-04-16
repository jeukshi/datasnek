{-# LANGUAGE DerivingVia #-}

module Message where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Text (Text)

newtype Message = MkMessage {text :: Text}
    deriving (Eq) via Text

instance FromJSON Message where
    parseJSON = withObject "Comment" $ \v ->
        MkMessage
            <$> v .: "comment"
