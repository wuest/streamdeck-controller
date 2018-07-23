module Decklist.Comm exposing (..)

import Json.Decode    as Json
import Decklist.Types as DL

decklistFrom : Json.Decoder DL.Decklist
decklistFrom = Json.at [ "decks" ] (Json.list deckinfoFrom)

deckinfoFrom : Json.Decoder DL.Deckinfo
deckinfoFrom = Json.map2 DL.Deckinfo ( Json.at [ "name" ]   Json.string )
                                     ( Json.at [ "serial" ] Json.string )
