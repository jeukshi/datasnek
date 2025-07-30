{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}
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
import Datastar qualified
import Html qualified
import Lucid hiding (for_)
import Queue
import RawSse
import Sleep
import Store
import Types

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
    (.chatMode) <$> getSettings storeRead >>= \case
        ChatOn -> chatVisible newMessage oldMessages
        ChatCommands -> chatVisible newMessage oldMessages
        ChatOff -> do
            chatHtml <- Html.chatMessages []
            let rawEvent = Datastar.patchElements chatHtml
            putChatContent storeWrite rawEvent
            putChatContentHtml storeWrite chatHtml
            putChatMessages storeWrite []
            _ <- tryWriteQueue mainPageQueue ()
            writeBroadcast broadcastServer (ChatFrameUpdate rawEvent)
  where
    chatVisible newMessage oldMessages = do
        anonymousMode <- (.anonymousMode) <$> getSettings storeRead
        let newMessageForChat =
                if anonymousMode
                    then (MkUser "snek" "snek", snd newMessage)
                    else newMessage
        -- CSS will reverse our list.
        let newMessagesForChat = take 20 (newMessageForChat : oldMessages)
        chatHtml <- Html.chatMessages newMessagesForChat
        let rawEvent = Datastar.patchElements chatHtml
        let rawMessageEvent =
                Datastar.patchElementsPrepend
                    "#chat-messages"
                    (Html.renderMessage newMessageForChat)
        putChatMessages storeWrite newMessagesForChat
        putChatContentHtml storeWrite chatHtml
        putChatContent storeWrite rawEvent
        _ <- tryWriteQueue mainPageQueue ()
        writeBroadcast broadcastServer (ChatNewMessage rawMessageEvent)
