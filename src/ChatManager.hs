module ChatManager (run) where

import Bluefin.Eff
import Bluefin.IO
import Bluefin.Internal qualified
import Bluefin.State
import Broadcast
import Color (assignColor)
import Control.Monad (forever, when)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (for_)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Lucid hiding (for_)
import Message
import Queue
import RawSse
import RenderHtml qualified
import Sleep
import Store
import StoreUpdate
import User

run
    :: (e1 :> es, e2 :> es, e3 :> es, e4 :> es, e5 :> es)
    => StoreWrite e1
    -> StoreRead e2
    -> Queue (User, Message) e3
    -> BroadcastServer StoreUpdate e4
    -> Queue () e5
    -> Eff es ()
run storeWrite storeRead queue broadcastServer mainPageQueue = forever do
    newMessage <- readQueue queue
    oldMessages <- getChatMessages storeRead
    getDisableChat storeRead >>= \case
        False -> do
            -- CSS will reverse our list.
            let newMessages = take 20 (newMessage : oldMessages)
            chatHtml <- RenderHtml.chatMessages newMessages
            let rawEvent = renderChatToRawEvent chatHtml
            let rawMessageEvent = renderMessageToRawEvent newMessage
            putChatMessages storeWrite newMessages
            putChatContentHtml storeWrite chatHtml
            putChatContent storeWrite rawEvent
            _ <- tryWriteQueue mainPageQueue ()
            writeBroadcast broadcastServer (ChatNewMessage rawMessageEvent)
        True -> do
            chatHtml <- RenderHtml.chatMessages []
            let rawEvent = renderChatToRawEvent chatHtml
            putChatContent storeWrite rawEvent
            putChatContentHtml storeWrite chatHtml
            putChatMessages storeWrite []
            _ <- tryWriteQueue mainPageQueue ()
            writeBroadcast broadcastServer (ChatFrameUpdate rawEvent)

renderChatToRawEvent :: Html () -> RawEvent
renderChatToRawEvent chatHtml =
    MkRawEvent $
        "event:datastar-patch-elements\n"
            <> "data:elements "
            <> renderBS chatHtml
            <> "\n"

renderMessageToRawEvent :: (User, Message) -> RawEvent
renderMessageToRawEvent messages =
    MkRawEvent $
        "event:datastar-patch-elements\n"
            <> "data: selector #chat-messages\n"
            <> "data: mode prepend\n"
            <> "data:elements "
            <> renderBS (RenderHtml.renderMessage messages)
            <> "\n"
