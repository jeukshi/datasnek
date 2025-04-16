module JavaScript where

import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)

windowLocationLogin :: BL.ByteString
windowLocationLogin = "window.location = \"/login\""

windowLocationMain :: BL.ByteString
windowLocationMain = "window.location = \"/\""

windowLocationReload :: BL.ByteString
windowLocationReload = "window.location.reload()"

postInChat :: Text
postInChat = "$comment !== '' && evt.key === 'Enter' && !evt.shiftKey && @post('/api/chat')"

login :: Text
login = "@get('/api/login', {contentType: 'form'})"

getTransmittal :: Text
getTransmittal = "@get('/api/transmittal');"

postPlay :: Text
postPlay = "@post('/api/play')"

hotreload :: Text
hotreload = "@get('dev/hotreload')"

gameInput :: Text
gameInput =
    "$isplaying === 'true' && evt.key ==='ArrowLeft' && @get('/api/game/change-direction/l');"
        <> "$isplaying === 'true' && evt.key ==='ArrowRight' && @get('/api/game/change-direction/r');"
        <> "$isplaying === 'true' && evt.key ==='ArrowUp' && @get('/api/game/change-direction/u');"
        <> "$isplaying === 'true' && evt.key ==='ArrowDown' && @get('/api/game/change-direction/d');"
