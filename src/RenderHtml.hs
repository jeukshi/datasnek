module RenderHtml where

import Bluefin.Eff
import Color (assignColor)
import Data.Foldable (for_)
import Data.Functor.Identity (Identity)
import JavaScript qualified
import Lucid hiding (for_)
import Lucid.Datastar
import Message
import User

chatMessages :: [(User, Message)] -> Eff es (Html ())
chatMessages userMessages = return do
    div_ [id_ "chat-messages", class_ "chat-messages"] do
        for_ userMessages \userMessage -> do
            renderMessage userMessage

renderMessage :: (User, Message) -> HtmlT Identity ()
renderMessage (user, message) = do
    let userColor = assignColor (userIdToText user.userId)
    div_ [class_ "chat-message"] do
        span_
            [class_ "chat-username", style_ ("color: " <> userColor <> ";")]
            (toHtml (user.name <> ": "))
        span_ [class_ "chat-content"] (toHtml message.text)
