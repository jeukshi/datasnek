module Sleep where

import Bluefin.Compound (Handle (..), useImpl, useImplIn)
import Bluefin.Eff (Eff, (:&), (:>))
import Bluefin.Extra
import Bluefin.IO (IOE, effIO)
import Bluefin.State
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import GHC.Base (when)
import System.Clock (Clock (Monotonic), getTime, toNanoSecs)

newtype Sleep e = UnsafeMkSleep {ioe :: IOE e}

instance ThreadSafe Sleep where
    accessConcurrently access (UnsafeMkSleep io) =
        UnsafeMkSleep <$> accessConcurrently access io

runSleep
    :: (e1 :> es)
    => IOE e1
    -> (forall e. Sleep e -> Eff (e :& es) r)
    -> Eff es r
runSleep io action =
    useImplIn action (UnsafeMkSleep (mapHandle io))

sleepMs :: (e :> es) => Sleep e -> Int -> Eff es ()
sleepMs (UnsafeMkSleep io) ms = effIO io do threadDelay (ms * 1000)

foreverWithSleep :: (e :> es) => Sleep e -> Int -> Eff es Int -> Eff es ()
foreverWithSleep (UnsafeMkSleep io) initialIntervalMs action = do
    let initialIntervalNano = initialIntervalMs * 1_000_1000
    evalState initialIntervalNano \intervalNsS -> forever do
        startTime <- effIO io do getTime Monotonic
        nextIntervalMs <- useImpl action
        endTime <- effIO io do getTime Monotonic
        let elapsedNs = toNanoSecs (endTime - startTime)
        intervalNs <- get intervalNsS
        let remainingNs = intervalNs - fromIntegral elapsedNs
        when (remainingNs > 0) $ do
            let delayUs = remainingNs `div` 1000
            effIO io do threadDelay delayUs
        put intervalNsS (max 0 (nextIntervalMs * 1_000_000))
