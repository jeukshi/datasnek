{-# LANGUAGE DerivingVia #-}

module Bluefin.Servant (streamToSourceIO, singleToSourceIO) where

import Bluefin.Compound (useImplUnder)
import Bluefin.Coroutine (forEach)
import Bluefin.Eff (Eff, (:&), (:>))
import Bluefin.IO (IOE, effIO)
import Bluefin.Internal qualified
import Bluefin.Stream (Stream)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Servant.API.Stream (SourceIO)
import Servant.Types.SourceT qualified as S

streamToSourceIO
    :: forall es a
     . (forall e. Stream a e -> Eff (e :& es) ())
    -> Eff es (SourceIO a)
streamToSourceIO stream = Bluefin.Internal.unsafeProvideIO \io -> do
    mvar <- effIO io do newEmptyMVar
    let produce :: forall e. IOE e -> Eff (e :& es) ()
        produce io' = do
            forEach (useImplUnder . stream) \a -> effIO io' do
                putMVar mvar (Just a)
            effIO io' do putMVar mvar Nothing

    _ <- inBackground (useImplUnder . produce) io
    pure $ S.SourceT $ \k -> k (consume mvar)
  where
    consume :: MVar (Maybe a) -> S.StepT IO a
    consume mv = S.Effect $ do
        mx <- takeMVar mv
        case mx of
            Nothing -> pure S.Stop
            Just x -> pure $ S.Yield x (consume mv)
    inBackground
        :: (e2 :> es2)
        => (forall e. IOE e -> Eff (e :& es2) ())
        -> IOE e2
        -> Eff es2 (Async.Async ())
    inBackground x io = do
        Bluefin.Internal.withEffToIO' io $ \toIO ->
            Async.async (toIO x)

singleToSourceIO :: a -> Eff es (SourceIO a)
singleToSourceIO x = pure $ S.SourceT \k -> k (S.Yield x S.Stop)
