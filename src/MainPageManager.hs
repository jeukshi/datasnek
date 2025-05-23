module MainPageManager (run) where

import Bluefin.Eff
import Control.Monad (forever)
import Data.Text (Text)
import JavaScript qualified
import Lucid
import Lucid.Datastar
import Queue
import Store

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
    settings <- getSettingsHtml storeRead
    pure do
        pageHead do
            main_
                [ class_ "container"
                , dataOnLoad_ JavaScript.hotreload
                , dataSignals_ "username" ""
                , dataSignals_ "isplaying" "false"
                , dataSignals_ "showchat" (if disableChat then "false" else "true")
                ]
                do
                    div_
                        [ class_ "game-container"
                        , dataOnLoad_ JavaScript.getTransmittal
                        , dataOnKeydown__window_ JavaScript.gameInput
                        ]
                        do
                            div_ [class_ "header"] do
                                h1_ "Datasnek"
                                button_ [class_ "play-button", dataOnMousedown_ JavaScript.postPlay] "Play"
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

pageHead :: (Monad m) => HtmlT m a -> HtmlT m a
pageHead main = doctypehtml_ do
    head_ do
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        meta_ [name_ "color-scheme", content_ "light dark"]
        link_ [rel_ "icon", type_ "image/png", href_ "/favicon.ico"]
        link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css"]
        link_ [rel_ "stylesheet", href_ "snek.css"]
        link_
            [ rel_ "stylesheet"
            , href_
                "https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@40,400,0,0&icon_names=cancel"
            ]
        script_
            [ type_
                "module"
            , src_
                "https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.0-beta.11/bundles/datastar.js"
            ]
            ("" :: Text)
        script_ [src_ "snek-game-component.js"] ("" :: Text)
        title_ "Datasnek"
    body_ do
        main
