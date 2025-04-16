module Bluefin.Proxy where

import Bluefin.Eff (Eff)
import Data.Proxy (Proxy (..))

captureEffEnv :: forall es. Eff es (Proxy es)
captureEffEnv = pure $ Proxy @es
