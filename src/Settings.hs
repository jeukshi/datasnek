module Settings where

import Bluefin.Eff
import Data.Text (Text)
import Data.Text qualified as T
import Lucid
import RawSse
import Store

renderSettingsToRawEvent :: (e :> es) => StoreRead e -> Eff es RawEvent
renderSettingsToRawEvent store = do
    settingsHtml <- renderSettings store
    pure $
        MkRawEvent $
            "event:datastar-merge-fragments\n"
                <> "data:fragments "
                <> renderBS settingsHtml
                <> "\n"

renderSettingItem :: Text -> Text -> Html ()
renderSettingItem label value =
    div_ [class_ "setting-item"] $ do
        span_ [class_ "setting-label"] $ toHtml label
        span_ [class_ "setting-value"] $ toHtml value

renderSettings :: (e :> es) => StoreRead e -> Eff es (Html ())
renderSettings store = do
    maxFood <- getMaxFood store
    maxPlayers <- getMaxPlayers store
    queueMaxSize <- getQueueMaxSize store
    boardSize <- getBoardSize store

    return $ div_ [id_ "settings", class_ "settings"] do
        div_ [class_ "settings-grid"] do
            renderSettingItem "Max Food:" (showT maxFood)
            renderSettingItem "Max Players:" (showT maxPlayers)
            renderSettingItem "Queue Size:" (showT queueMaxSize)
            renderSettingItem "Board Size:" (showT boardSize)

showT :: (Show a) => a -> Text
showT = T.pack . show
