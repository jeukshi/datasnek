{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module RawSse where

import Data.ByteString.Lazy qualified as BL
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import Servant (HasLink, HasServer (..), NewlineFraming)
import Servant.API (
    Accept,
    GetHeaders,
    HasLink (..),
    Headers,
    MimeRender,
    StreamGet,
    StreamPost,
    ToSourceIO,
 )
import Servant.API.ContentTypes (Accept (..), MimeRender (..))

newtype RawEvent = MkRawEvent BL.ByteString
    deriving (Show) via BL.ByteString

instance ToSse RawEvent where
    toSse = coerce

-- This is inspired by `servant-event-stream`, but doesn't touch
-- event data at all. Which means we need to write `ToSse` instance
-- correctly, but we don't do any computation per client later.
-- TODO add headers!
data Sse

instance Accept Sse where
    contentType _ = "text" // "event-stream" /: ("charset", "utf-8")

class ToSse a where
    toSse :: a -> BL.ByteString

instance (ToSse a) => MimeRender Sse a where
    mimeRender _ = toSse

data SseGet (a :: Type)
    deriving stock (Typeable, Generic)

instance HasLink (SseGet a) where
    type MkLink (SseGet a) r = r
    toLink toA _ = toA

-- TODO is NewlineFraming necessary? Probably, sadly. Figure out when that happens.
instance
    {-# OVERLAPPABLE #-}
    (ToSse chunk, ToSourceIO chunk a)
    => HasServer (SseGet a) context
    where
    type ServerT (SseGet a) m = ServerT (StreamGet NewlineFraming Sse a) m
    route Proxy =
        route
            (Proxy :: Proxy (StreamGet NewlineFraming Sse a))
    hoistServerWithContext Proxy =
        hoistServerWithContext
            (Proxy :: Proxy (StreamGet NewlineFraming Sse a))

instance
    {-# OVERLAPPING #-}
    (ToSse chunk, ToSourceIO chunk a, GetHeaders (Headers h a))
    => HasServer (SseGet (Headers h a)) context
    where
    type
        ServerT (SseGet (Headers h a)) m =
            ServerT (StreamGet NewlineFraming Sse (Headers h a)) m
    route Proxy =
        route
            (Proxy :: Proxy (StreamGet NewlineFraming Sse (Headers h a)))
    hoistServerWithContext Proxy =
        hoistServerWithContext
            (Proxy :: Proxy (StreamGet NewlineFraming Sse (Headers h a)))

data SsePost (a :: Type)
    deriving stock (Typeable, Generic)

instance HasLink (SsePost a) where
    type MkLink (SsePost a) r = r
    toLink toA _ = toA

-- TODO is NewlineFraming necessary? Probably, sadly. Figure out when that happens.
instance
    {-# OVERLAPPABLE #-}
    (ToSse chunk, ToSourceIO chunk a)
    => HasServer (SsePost a) context
    where
    type ServerT (SsePost a) m = ServerT (StreamPost NewlineFraming Sse a) m
    route Proxy =
        route
            (Proxy :: Proxy (StreamPost NewlineFraming Sse a))
    hoistServerWithContext Proxy =
        hoistServerWithContext
            (Proxy :: Proxy (StreamPost NewlineFraming Sse a))

instance
    {-# OVERLAPPING #-}
    (ToSse chunk, ToSourceIO chunk a, GetHeaders (Headers h a))
    => HasServer (SsePost (Headers h a)) context
    where
    type
        ServerT (SsePost (Headers h a)) m =
            ServerT (StreamPost NewlineFraming Sse (Headers h a)) m
    route Proxy =
        route
            (Proxy :: Proxy (StreamPost NewlineFraming Sse (Headers h a)))
    hoistServerWithContext Proxy =
        hoistServerWithContext
            (Proxy :: Proxy (StreamPost NewlineFraming Sse (Headers h a)))
