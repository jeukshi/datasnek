module Datastar where

import Data.Aeson qualified as Json
import Lucid (Html, renderBS)
import RawSse (RawEvent (MkRawEvent))

patchElements :: Html () -> RawEvent
patchElements html =
    MkRawEvent $
        "event:datastar-patch-elements\n"
            <> "data:elements "
            <> renderBS html
            <> "\n"

patchSignals :: Json.Value -> RawEvent
patchSignals json =
    MkRawEvent $
        "event:datastar-patch-signals\n"
            <> "data:signals "
            <> Json.encode json
            <> "\n"
