module Streamdeck.Comm exposing (..)

import WebSocket   as WS
import Json.Decode as Json

import Streamdeck.Types as SD

streamdeckFrom : Json.Decoder SD.Streamdeck
streamdeckFrom = Json.map2 SD.Streamdeck ( Json.at [ "name" ]   Json.string )
                                         ( Json.at [ "serial" ] Json.string )
