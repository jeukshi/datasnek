{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Bluefin.Extra where

import Bluefin.Compound
import Bluefin.Concurrent.Local qualified as BC
import Bluefin.Eff (Eff, (:&), (:>))
import Bluefin.IO (IOE)
import Bluefin.Internal qualified

useImplIn2
    :: (e :> es)
    => (t1 -> t2 -> Eff (es :& e) r)
    -> t1
    -> t2
    -> Eff es r
useImplIn2 f h1 h2 = Bluefin.Internal.inContext (f h1 h2)

useImplIn3
    :: (e :> es)
    => (t1 -> t2 -> t3 -> Eff (es :& e) r)
    -> t1
    -> t2
    -> t3
    -> Eff es r
useImplIn3 f h1 h2 h3 = Bluefin.Internal.inContext (f h1 h2 h3)

class ThreadSafe a where
    accessConcurrently :: (e' :> es', e :> es) => BC.ExclusiveAccess es' e -> a e' -> Eff es (a e)

instance ThreadSafe IOE where
    accessConcurrently _ _ = pure Bluefin.Internal.MkIOE

instance ThreadSafe BC.STME where
    accessConcurrently _ _ = pure BC.UnsafeMkSTME

instance ThreadSafe BC.NonDet where
    accessConcurrently access (BC.UnsafeMkNonDet io) =
        BC.UnsafeMkNonDet <$> accessConcurrently access io

-- FIXME Is is worth making sure here that `scopeEs` and `es'` are the same?
instance ThreadSafe (BC.Scope scopeEs) where
    accessConcurrently
        :: (e' :> es', e :> es)
        => BC.ExclusiveAccess es' e
        -> BC.Scope scopeEs e'
        -> Eff es (BC.Scope scopeEs e)
    accessConcurrently (BC.UnsafeMkExclusiveAccess x y z) (BC.UnsafeMkScope scope _) =
        pure (BC.UnsafeMkScope scope (BC.UnsafeMkExclusiveAccess x y z))

-- FIXME This thing is probably wrong and I don't like it.
-- But it fixes my problem.
-- The problem is as follows:
-- When using `accessConcurrently` effect gets new tag (from Access).
-- But then the relation between `scope` and that new tag is lost.
-- If I want to pass `scope` and some other handle to a function
-- I need to prove that (e :> scopeEs)
-- This might be combined with `accessConcurrently` into one function.
growScope
    :: (e1 :> es)
    => BC.Scope oldScopeEs e1
    -> (forall e. BC.Scope (oldScopeEs :& es) e -> Eff (e :& es) r)
    -> Eff es r
growScope (BC.UnsafeMkScope scope (BC.UnsafeMkExclusiveAccess x y z)) action =
    useImplIn action (BC.UnsafeMkScope scope (BC.UnsafeMkExclusiveAccess x (mapHandle y) (mapHandle z)))
