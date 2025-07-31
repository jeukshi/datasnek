{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Bluefin.Concurrent.Local where

import Bluefin.Internal
import Control.Concurrent (MVar)
import Control.Concurrent.MVar (newEmptyMVar)
import Control.Concurrent.STM qualified as STM
import Ki qualified

-- This is all stolen copy/pasted code from Bluefin ki branch,
-- since I had to change `scoped` definition a little bit
data ExclusiveAccess (es' :: Effects) (es :: Effects)
    = UnsafeMkExclusiveAccess (MVar ()) (STME es) (NonDet es)

instance Handle (ExclusiveAccess es') where
    mapHandle (UnsafeMkExclusiveAccess v stm nonDet) =
        UnsafeMkExclusiveAccess v (mapHandle stm) (mapHandle nonDet)

data Scope (es' :: Effects) (es :: Effects)
    = UnsafeMkScope Ki.Scope (ExclusiveAccess es' es)

instance Handle (Scope es') where
    mapHandle (UnsafeMkScope scope excl) =
        UnsafeMkScope scope (mapHandle excl)

newtype Thread a (e :: Effects)
    = UnsafeMkThread (Ki.Thread a)

instance Handle (Thread r) where
    mapHandle (UnsafeMkThread t) = UnsafeMkThread t

newtype NonDet e = UnsafeMkNonDet (IOE e)

instance Handle NonDet where
    mapHandle (UnsafeMkNonDet io) = UnsafeMkNonDet (mapHandle io)

withNonDet
    :: (e1 :> es)
    => IOE e1
    -> (forall e. NonDet e -> Eff (e :& es) r)
    -> Eff es r
withNonDet io k = useImplIn k (UnsafeMkNonDet (mapHandle io))

scoped
    :: (e1 :> es)
    => NonDet e1
    -- I had to change this
    -- -> (forall e. Scope es e -> Eff e r)
    -- into this, so that scope is included in it's own scope!
    -> (forall e. Scope (e :& es) e -> Eff e r)
    -> Eff es r
    -- ^ ͘
scoped nonDet@(UnsafeMkNonDet io) k =
    withEffToIO_ io $ \effToIO -> Ki.scoped $ \scope -> do
        -- Unlocked when it's empty
        lock <- newEmptyMVar
        effToIO
            ( k
                ( UnsafeMkScope
                    scope
                    (UnsafeMkExclusiveAccess lock UnsafeMkSTME (mapHandle nonDet))
                )
            )

fork
    :: (e1 :> es)
    => Scope es' e1
    -> (forall e. ExclusiveAccess es' e -> Eff e r)
    -> Eff es (Thread r es)
    -- ^ ͘
fork (UnsafeMkScope scope (UnsafeMkExclusiveAccess lock stm nonDet)) body = do
    thread <- UnsafeMkEff $ Ki.fork scope $ do
        unsafeUnEff (body (UnsafeMkExclusiveAccess lock stm nonDet))
    pure (UnsafeMkThread thread)

awaitEff
    :: (e :> es)
    => Thread a e
    -> Eff es a
    -- ^ ͘
awaitEff (UnsafeMkThread t) =
    -- This is safe, presumably, because it doesn't change any STM
    -- variables observably, presumably.
    UnsafeMkEff (STM.atomically (Ki.await t))

newtype EffSTM (e :: Effects) a = UnsafeMkEffSTM {unsafeUnEffSTM :: STM.STM a}

data STME (e :: Effects) = UnsafeMkSTME

instance Handle STME where
    mapHandle UnsafeMkSTME = UnsafeMkSTME

atomicallySTM
    :: (e1 :> es)
    => STME e1
    -> STM.STM r
    -> Eff es r
    -- ^ ͘
atomicallySTM UnsafeMkSTME stm = UnsafeMkEff (STM.atomically stm)

runSTM
    :: (e1 :> es)
    => IOE e1
    -> (forall e. STME e -> Eff (e :& es) r)
    -> Eff es r
    -- ^ ͘
runSTM MkIOE body = makeOp (body UnsafeMkSTME)
