module Html where

import Bluefin.Eff
import Color (assignColor)
import Control.Monad (unless)
import Data.Aeson (KeyValue ((.=)))
import Data.Aeson qualified as Aeson
import Data.Aeson qualified as Json
import Data.Aeson.Key qualified as Json
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (for_)
import Data.Functor.Identity (Identity)
import Data.Strict.Map (Map)
import Data.Strict.Map qualified as Map
import Data.Strict.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GameState
import JavaScript qualified
import Lucid hiding (for_)
import Lucid.Datastar
import Message
import Snek
import Types
import User
import WebComponents (
    anonymous_,
    boardSize_,
    food_,
    location_,
    snekGameBoard_,
    sneks_,
    windowController_,
 )

chatEnabled :: Html ()
chatEnabled = div_ [id_ "chat", class_ "chat"] do
    label_ [] do
        input_ [type_ "checkbox", role_ "switch", checked_, dataBind_ "chaton"]
        "chat"
    div_ [id_ "chat-messages", class_ "chat-messages", dataShow_ "$chaton"] mempty
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
            ]

chatDisabled :: Html ()
chatDisabled = div_ [id_ "chat", class_ "chat"] do
    label_ [] do
        input_ [type_ "checkbox", role_ "switch", checked_, dataBind_ "chaton"]
        "chat"
    div_ [id_ "chat-messages", class_ "chat-messages", hidden_ ""] mempty

chatMessages :: [(User, Message)] -> Eff es (Html ())
chatMessages userMessages = return do
    div_ [id_ "chat-messages", class_ "chat-messages"] do
        for_ userMessages \userMessage -> do
            renderMessage userMessage

renderMessage :: (User, Message) -> Html ()
renderMessage (user, message) = do
    let userColor = assignColor (userIdToText user.userId)
    div_ [class_ "chat-message"] do
        span_
            [class_ "chat-username", style_ ("color: " <> userColor <> ";")]
            (toHtml (user.name <> ": "))
        span_ [class_ "chat-content"] (toHtml message.text)

settings :: Bool -> Int -> Settings -> Html ()
settings queueFull alive settings = div_ [id_ "settings", class_ "settings"] do
    div_ [class_ "settings-grid"] do
        settingItem ("maxFood", T.pack . show $ settings.maxFood)
        settingItem ("maxPlayers", T.pack . show $ settings.maxPlayers)
        settingItem ("playing", T.pack . show $ alive)
        settingItem ("queueMaxSize", T.pack . show $ settings.queueMaxSize)
        settingItem ("boardSize", T.pack . show $ settings.boardSize)
        settingItem ("gameFrameTimeMs", T.pack . show $ settings.gameFrameTimeMs)
        settingItem ("useWebComponent", if settings.useWebComponent then "true" else "false")
        settingItem ("anonymousMode", if settings.anonymousMode then "true" else "false")
        settingItem ("disableChat", if settings.disableChat then "true" else "false")
        settingItem ("queueFull", if queueFull then "true" else "false")
        settingItem ("maxBots", T.pack . show $ settings.maxBots)
        settingItem ("gracePeriod", T.pack . show $ settings.gracePeriod)
        settingItem ("snekSelfOwn", if settings.snekSelfOwn then "true" else "false")
        settingItem ("snekCanReverse", if settings.snekCanReverse then "true" else "false")

settingItem :: (Text, Text) -> Html ()
settingItem (label, value) =
    div_ [class_ "setting-item"] $ do
        span_ [class_ "setting-label"] $ toHtml label
        span_ [class_ "setting-value"] $ toHtml value

leaderboard :: Bool -> Sneks -> Sneks -> Html ()
leaderboard anonymousMode currentSneks allTimeSneks = do
    div_ [id_ "leaderboard", class_ "leaderboard"] $ do
        div_ [class_ "leaderboard-section"] do
            h3_ [] $ toHtml ("All-Time Top 5" :: Text)
            div_ [class_ "leaderboard-entries"] $
                mconcat $
                    zipWith (leaderboardEntry anonymousMode) [1 ..] allTimeSneks
            hr_ []
        div_ [class_ "leaderboard-section"] do
            div_ [class_ "leaderboard-entries"] $
                mconcat $
                    zipWith (leaderboardEntry anonymousMode) [1 ..] currentSneks

leaderboardEntry :: Bool -> Int -> Snek -> Html ()
leaderboardEntry anonymousMode position snek =
    div_ [class_ "leaderboard-entry"] do
        div_ [class_ "leaderboard-rank"] do
            div_ [class_ "leaderboard-position"] do toHtml (show position <> ".")
            div_ [class_ "leaderboard-score"] do toHtml (show $ snekLength snek)
        div_
            [class_ "leaderboard-player", style_ $ "color: " <> assignColor (userIdToText snek.user.userId)]
            $ if anonymousMode then "snek" else toHtml snek.user.name
  where
    snekLength :: Snek -> Int
    snekLength snek = 1 + length snek.restOfSnek -- +1 for head.

pageHead :: Html () -> Html ()
pageHead main = doctypehtml_ do
    head_ do
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        meta_ [name_ "color-scheme", content_ "light dark"]
        link_ [rel_ "icon", type_ "image/png", href_ "/favicon.ico"]
        link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css"]
        link_ [rel_ "stylesheet", href_ "snek.css"]
        script_
            [ type_
                "module"
            , src_
                "https://cdn.jsdelivr.net/gh/starfederation/datastar@main/bundles/datastar.js"
            ]
            ("" :: Text)
        script_ [src_ "snek-game-component.js"] ("" :: Text)
        script_ [src_ "window-controller.js"] ("" :: Text)
        title_ "Datasnek"
    body_ do
        main

loginPage :: Html ()
loginPage = pageHead do
    main_ [class_ "container"] do
        div_ [id_ "screen"] do
            form_ [dataOnSubmit_ JavaScript.login] do
                input_ [name_ "username", placeholder_ "Username", required_ ""]
                button_ "join"
                windowController_ [id_ "window-controller"] mempty

mainPage :: Html ()
mainPage = pageHead do
    main_
        [ class_ "container"
        , dataSignalsJson_
            ( Json.object
                [ "username" .= ("" :: Text)
                , "isplaying" .= False
                , "inqueue" .= False
                ]
            )
        ]
        do
            div_ [dataOnLoad_ JavaScript.hotreload] mempty
            div_ [dataOnLoad_ JavaScript.getTransmittal] mempty
            div_ [dataOnKeydown__window_ JavaScript.gameInput] mempty
            div_ [class_ "main-container"] do
                div_ [id_ "frame-container", class_ "frame-container"] do
                    div_ [class_ "header"] do
                        h1_ "Datasnek"
                    div_ [id_ "game-area", class_ "game-area"] mempty
                chatDisabled
            windowController_ [id_ "window-controller"] mempty

renderFrame :: Bool -> Html () -> Html () -> Html () -> Html ()
renderFrame isQueueFull settings leaderboard board = div_ [id_ "frame-container", class_ "frame-container"] do
    div_ [class_ "header"] do
        h1_ "Datasnek"
        unless isQueueFull do
            button_
                [ id_ "play-button"
                , class_ "play-button"
                , dataOnMousedown_ JavaScript.postPlay
                , dataAttr_ "disabled" "$inqueue"
                ]
                "Play"
    div_ [id_ "game-area", class_ "game-area"] do
        leaderboard
        board
    settings

renderBoardWebComponent :: Bool -> GameState -> Html ()
renderBoardWebComponent anonymousMode gameState = do
    let foodJson = encodeToText . Aeson.toJSON . Set.toList $ gameState.foodPositions
    let snakesJson = encodeToText . Aeson.toJSON $ map snekToObject gameState.aliveSneks
    let boardSizeTxt = toText gameState.boardSize
    div_ [id_ "board", class_ "board"] do
        snekGameBoard_
            [ id_ "snek-game-board"
            , boardSize_ boardSizeTxt
            , food_ foodJson
            , sneks_ snakesJson
            , dataAttr_ "username" "$username"
            , anonymous_ anonymousMode
            ]
            mempty

renderBoard :: Bool -> User -> GameState -> Html ()
renderBoard anonymousMode renderForUser gameState = do
    div_ [id_ "board", class_ "board"] do
        sequence_
            [ div_ [class_ "board-container"] do
                sequence_
                    [ case Map.lookup (c, r) snakeCells of
                        (Just (user, color, isHead, gracePeriod)) -> do
                            let opacity = calculateGracePeriodOpacity gracePeriod
                            let opacityStyle =
                                    if opacity < 1.0
                                        then ";opacity:" <> T.pack (show opacity)
                                        else ""
                            let styleAttr = "background-color:" <> color <> opacityStyle
                            div_ [class_ (base <> " snake"), style_ styleAttr] do
                                let username = if anonymousMode then "snek" else toHtml user.name
                                if isHead
                                    then
                                        if renderForUser.userId == user.userId
                                            then div_ [class_ "nameplate-me"] username
                                            else div_ [class_ "nameplate"] username
                                    else mempty
                        Nothing -> do
                            let cls =
                                    if (c, r) `elem` gameState.foodPositions
                                        then base <> " food"
                                        else base
                            div_ [class_ cls] mempty
                    | c <- [0 .. gameState.boardSize]
                    ]
            | r <- [0 .. gameState.boardSize]
            ]
  where
    calculateGracePeriodOpacity :: Int -> Double
    calculateGracePeriodOpacity gracePeriod
        | gracePeriod <= 0 = 1.0
        | gracePeriod >= 5 = 0.5
        | otherwise = 0.5 + (0.1 * fromIntegral (5 - gracePeriod))
    snakeCells :: Map (Int, Int) (User, Text, Bool, Int)
    snakeCells =
        Map.fromList $
            [ (coord, (s.user, s.color, False, s.gracePeriod))
            | s <- gameState.aliveSneks
            , coord <- s.restOfSnek
            ]
                -- It is important that heads go last into the list.
                -- That way head will be always in the map,
                -- so we can display nameplate properly.
                ++ [ (s.headOfSnek, (s.user, s.color, True, s.gracePeriod))
                   | s <- gameState.aliveSneks
                   ]
    base = "board-item"

encodeToText :: Aeson.Value -> Text
encodeToText = T.decodeUtf8 . BL.toStrict . Aeson.encode

snekToObject :: Snek -> Aeson.Value
snekToObject (MkSnek (MkUser name userId) color (hx, hy) rest gracePeriod) =
    Aeson.object
        [ "username" .= Aeson.String name
        , "color" .= Aeson.String color
        , "headOfSnek" .= Aeson.toJSON (hx, hy)
        , "restOfSnek" .= Aeson.toJSON rest
        , "gracePeriod" .= Aeson.toJSON gracePeriod
        ]

toText :: (Show a) => a -> Text
toText = T.pack . show
