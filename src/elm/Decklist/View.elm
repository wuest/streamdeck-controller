module Decklist.View exposing (root)

import Html             exposing (..)
import Html.Attributes  exposing (..)
import Decklist.Types   exposing (..)

root : Decklist -> Html Msg
root xs =
    select [ class "decklist" ] <|
        case xs of
            [] -> [ option  [ value "", disabled True, selected True ] [ text "(No Streamdecks detected)" ] ]
            _  -> (List.map (\n -> option [ value <| .serial n ] [ text <| .name n ]) xs)
