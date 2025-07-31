module GenUuid where

import Bluefin.Compound (Handle (..), useImplIn)
import Bluefin.Eff (Eff, (:&), (:>))
import Bluefin.Extra
import Bluefin.IO (IOE, effIO)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)

newtype GenUuid e = UnsafeMkGenUuid {ioe :: IOE e}

instance ThreadSafe GenUuid where
    accessConcurrently access (UnsafeMkGenUuid io) =
        UnsafeMkGenUuid <$> accessConcurrently access io

runGenUuid
    :: (e1 :> es)
    => IOE e1
    -> (forall e. GenUuid e -> Eff (e :& es) r)
    -> Eff es r
runGenUuid io action =
    useImplIn action (UnsafeMkGenUuid (mapHandle io))

nextUuid :: (e :> es) => GenUuid e -> Eff es UUID
nextUuid (UnsafeMkGenUuid io) = effIO io do nextRandom
