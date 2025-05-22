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
