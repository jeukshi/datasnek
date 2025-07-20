module MainPageManager (run) where

import Bluefin.Eff
import Control.Monad (forever)
import Data.Text (Text)
import JavaScript qualified
import Lucid
import Lucid.Datastar
import Queue
import RenderHtml qualified
import Store
import WebComponents (windowController_)

run
    :: (e1 :> es, e2 :> es, e3 :> es)
    => StoreWrite e1
    -> StoreRead e2
    -> Queue () e3
    -> Eff es ()
run storeWrite storeRead queue = forever do
    mainPage <- renderBS <$> renderPage storeRead
    putMainPageBS storeWrite mainPage
    _ <- readQueue queue
    pure ()

renderPage :: (e :> es) => StoreRead e -> Eff es (Html ())
renderPage storeRead = do
    -- Can you smell it? Yes, yes, race conditions.
    -- Unlucky if somebody gets a page with chat disabled,
    -- but misses the chat enabled event. They will feel lonely.
    -- F5 can fix it, but do they know? It's a web app,
    -- they should know that F5 fixes everything.
    chatMessages <- getChatContentHtml storeRead
    disableChat <- getDisableChat storeRead
    leaderboard <- getLeaderboardHtml storeRead
    isQueueFull <- getIsQueueFull storeRead
    settings <- getSettingsHtml storeRead
    pure do
        RenderHtml.pageHead do
            main_
                [ class_ "container"
                , dataSignals_ "username" ""
                , dataSignals_ "isplaying" "false"
                , dataSignals_ "showchat" (if disableChat then "false" else "true")
                , dataSignals_ "queuefull" (if isQueueFull then "true" else "false")
                ]
                do
                    div_ [dataOnLoad_ JavaScript.hotreload] mempty
                    div_ [dataOnLoad_ JavaScript.getTransmittal] mempty
                    div_ [dataOnKeydown__window_ JavaScript.gameInput] mempty
                    div_ [class_ "game-container"] do
                        div_
                            [class_ "header"]
                            do
                                h1_ "Datasnek"
                                button_
                                    [ id_ "play-button"
                                    , class_ "play-button"
                                    , dataOnMousedown_ JavaScript.postPlay
                                    , dataShow_ "!$queuefull"
                                    ]
                                    "Play"
                                div_ [id_ "queuefull", dataShow_ "$queuefull"] "queue full"
                        div_ [id_ "game-area", class_ "game-area"] do
                            leaderboard
                            div_ [id_ "board", class_ "board"] mempty
                        div_ [id_ "chat", class_ "chat"] do
                            chatMessages
                            div_ [dataSignals_ "comment" ""] mempty
                            div_ [id_ "chat-input-container", class_ "chat-input-container"] do
                                input_
                                    [ type_ "text"
                                    , dataBind_ "comment"
                                    , dataOnKeydown_ JavaScript.postInChat
                                    , class_ "chat-input"
                                    , name_ "message"
                                    , placeholder_ "Send a message..."
                                    , maxlength_ "300"
                                    , dataShow_ "$showchat"
                                    ]
                        settings
                    windowController_ [id_ "window-controller"] mempty
