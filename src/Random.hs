module Random where

import Bluefin.Compound (Handle (..), useImplIn)
import Bluefin.Eff (Eff, (:&), (:>))
import Bluefin.Extra
import Bluefin.IO (IOE, effIO)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import System.Random.Stateful (globalStdGen, uniformRM)

newtype Random e = UnsafeMkRandom {ioe :: IOE e}

instance ThreadSafe Random where
    accessConcurrently access (UnsafeMkRandom io) =
        UnsafeMkRandom <$> accessConcurrently access io

runRandom
    :: (e1 :> es)
    => IOE e1
    -> (forall e. Random e -> Eff (e :& es) r)
    -> Eff es r
runRandom io action =
    useImplIn action (UnsafeMkRandom (mapHandle io))

randomInExclude :: (e :> es) => Random e -> Int -> Int -> [(Int, Int)] -> Eff es (Int, Int)
randomInExclude (UnsafeMkRandom io) r c exclude = do
    let available = [(x, y) | x <- [0 .. r], y <- [0 .. c], (x, y) `notElem` exclude]
    i <- effIO io do uniformRM (0, length available - 1) globalStdGen
    pure (available !! i)

randomIn :: (e :> es) => Random e -> Int -> Int -> Eff es (Int, Int)
randomIn (UnsafeMkRandom io) r c = do
    -- TODO rewrite to not use list
    let available = [(x, y) | x <- [0 .. r], y <- [0 .. c]]
    i <- effIO io do uniformRM (0, length available - 1) globalStdGen
    pure (available !! i)

randomInt :: (e :> es) => Random e -> Int -> Int -> Eff es Int
randomInt (UnsafeMkRandom io) lo hi =
    effIO io (uniformRM (lo, hi) globalStdGen)

randomFromList :: (e :> es) => Random e -> NonEmpty a -> Eff es a
randomFromList random xs = do
    ix <- randomInt random 0 (NonEmpty.length xs - 1)
    pure $ xs NonEmpty.!! ix

random01 :: (e :> es) => Random e -> Eff es Float
random01 (UnsafeMkRandom io) = effIO io do uniformRM (0.0, 1.0) globalStdGen
