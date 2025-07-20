{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module Lucid.Datastar where

import Data.Text (Text)
import Lucid.Base (Attributes, makeAttributes)

dataSignals_ :: Text -> Text -> Attributes
dataSignals_ signalName = makeAttributes ("data-signals-" <> signalName)

dataSignalsJson_ :: Text -> Attributes
dataSignalsJson_ signalName = makeAttributes ("data-signals=" <> signalName) ""

dataText_ :: Text -> Attributes
dataText_ = makeAttributes "data-text"

dataOnClick_ :: Text -> Attributes
dataOnClick_ = makeAttributes "data-on-click"

dataOnMousedown_ :: Text -> Attributes
dataOnMousedown_ = makeAttributes "data-on-mousedown"

dataOnLoad_ :: Text -> Attributes
dataOnLoad_ = makeAttributes "data-on-load"

dataOnKeydown__window_ :: Text -> Attributes
dataOnKeydown__window_ = makeAttributes "data-on-keydown__window"

dataRef_ :: Text -> Attributes
dataRef_ name = makeAttributes ("data-ref-" <> name) ""

dataOnSubmit_ :: Text -> Attributes
dataOnSubmit_ = makeAttributes "data-on-submit"

dataBind_ :: Text -> Attributes
dataBind_ = makeAttributes "data-bind"

dataShow_ :: Text -> Attributes
dataShow_ = makeAttributes "data-show"

dataOnKeydown_ :: Text -> Attributes
dataOnKeydown_ = makeAttributes "data-on-keydown"

dataAttr_ :: Text -> Text -> Attributes
dataAttr_ signalName = makeAttributes ("data-attr-" <> signalName)

xxx :: Text -> Attributes
xxx = makeAttributes "data-on-datastar-fetch"
