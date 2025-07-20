module RenderHtml where

import Bluefin.Eff
import Color (assignColor)
import Data.Foldable (for_)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import JavaScript qualified
import Lucid hiding (for_)
import Lucid.Datastar
import Message
import Snek
import User
import WebComponents (location_, windowController_)

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

settings :: [(Text, Text)] -> Eff es (Html ())
settings settings = do
    return $ div_ [id_ "settings", class_ "settings"] do
        div_ [class_ "settings-grid"] do
            for_ settings \setting -> do
                settingItem setting

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

loginPage :: Eff es (Html ())
loginPage = return do
    pageHead do
        main_ [class_ "container"] do
            div_ [id_ "screen"] do
                form_ [dataOnSubmit_ JavaScript.login] do
                    input_ [name_ "username", placeholder_ "Username", required_ ""]
                    button_ "join"
                    windowController_ [id_ "window-controller"] mempty

pageHead :: (Monad m) => HtmlT m a -> HtmlT m a
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
