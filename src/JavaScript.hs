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
getTransmittal = "@get('/api/transmittal', {openWhenHidden: true});"

postPlay :: Text
postPlay = "@post('/api/play')"

hotreload :: Text
hotreload = "@get('/dev/hotreload');"

gameInput :: Text
gameInput =
    "$isplaying === true && ['ArrowLeft', 'a', 'h'].includes(evt.key) && @post('/api/game/change-direction/l');"
        <> "$isplaying === true && ['ArrowRight', 'd', 'l'].includes(evt.key) && @post('/api/game/change-direction/r');"
        <> "$isplaying === true && ['ArrowUp', 'w', 'k'].includes(evt.key) && @post('/api/game/change-direction/u');"
        <> "$isplaying === true && ['ArrowDown', 's', 'j'].includes(evt.key) && @post('/api/game/change-direction/d');"
