module Queue where

import Bluefin.Compound
import Bluefin.Concurrent.Local qualified as BC
import Bluefin.Eff
import Bluefin.Extra
import Bluefin.IO
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM.TBQueue (
    TBQueue,
    isFullTBQueue,
    lengthTBQueue,
    newTBQueue,
    readTBQueue,
    tryReadTBQueue,
    writeTBQueue,
 )
import Control.Concurrent.STM.TVar (TVar)
import Numeric.Natural (Natural)

-- TODO make size dynamic
data Queue a e = UnsafeMkQueue
    { stme :: BC.STME e
    , -- TBQueue is a bad choice because somebody might DOS our queue
      -- by entering multiple times. Thankfully, this is the internet,
      -- everybody will behave.
      queue :: TBQueue a
    }

instance ThreadSafe (Queue a) where
    accessConcurrently access (UnsafeMkQueue stme queue) =
        UnsafeMkQueue
            <$> accessConcurrently access stme
            <*> pure queue

runQueue
    :: (e1 :> es)
    => BC.STME e1
    -> Natural
    -> (forall e. Queue a e -> Eff (e :& es) r)
    -> Eff es r
runQueue stme maxSize action = do
    queue <- BC.atomicallySTM stme (newTBQueue maxSize)
    useImplIn action (UnsafeMkQueue (mapHandle stme) queue)

readQueue
    :: (e :> es) => Queue a e -> Eff es a
readQueue (UnsafeMkQueue stme queue) =
    BC.atomicallySTM stme do readTBQueue queue

tryReadQueue
    :: (e :> es) => Queue a e -> Eff es (Maybe a)
tryReadQueue (UnsafeMkQueue stme queue) =
    BC.atomicallySTM stme do tryReadTBQueue queue

writeQueue
    :: (e :> es) => Queue a e -> a -> Eff es ()
writeQueue (UnsafeMkQueue stme queue) new =
    BC.atomicallySTM stme do writeTBQueue queue new

isQueueFull
    :: (e :> es) => Queue a e -> Eff es Bool
isQueueFull (UnsafeMkQueue stme queue) =
    BC.atomicallySTM stme do isFullTBQueue queue

queueLength
    :: (e :> es) => Queue a e -> Eff es Natural
queueLength (UnsafeMkQueue stme queue) =
    BC.atomicallySTM stme do lengthTBQueue queue
