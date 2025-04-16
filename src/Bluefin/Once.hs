module Bluefin.Once where

import Bluefin.Compound (Handle (..), useImplIn)
import Bluefin.Eff (Eff, (:&), (:>))
import Bluefin.IO (IOE, effIO)
import Control.Concurrent (newMVar)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar)

data Once e = UnsafeMkOnce
    { ioe :: IOE e
    , block :: MVar ()
    }

runOnce
    :: (e1 :> es)
    => IOE e1
    -> (forall e. Once e -> Eff (e :& es) a)
    -> Eff es a
runOnce io action = do
    mvar <- effIO io do newMVar ()
    useImplIn action (UnsafeMkOnce (mapHandle io) mvar)

only
    :: (e :> es)
    => Once e
    -> Eff es a
    -> Eff es a
only (UnsafeMkOnce io mvar) action = do
    _ <- effIO io do takeMVar mvar
    action
