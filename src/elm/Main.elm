module Main exposing (main)

import Html            as H
import Html.Events     as Event
import Html.Attributes as Attr

import Streamdeck.View  as SD
import Streamdeck.Types as SD

import Decklist.View  as DL
import Decklist.Types as DL

type alias Model = { deckList : DL.Decklist
                   , currentDeck : Maybe SD.Streamdeck
                   }

type alias Flags = {}

type Msg = SDMsg SD.Msg
         | DLMsg DL.Msg

main : Program Flags Model Msg
main = H.programWithFlags
    { init          = init
    , update        = update
    , view          = view
    , subscriptions = subscriptions
    }

init : Flags -> (Model, Cmd Msg)
init _ = { currentDeck = Nothing
         , deckList = []
         } ! []

update : Msg -> Model -> (Model, Cmd Msg)
update _ m = m ! []

view : Model -> H.Html Msg
view m =
  H.div []
    [ H.div []
      [ H.map DLMsg (DL.root <| .deckList m) ]
    , H.div []
      [ H.map SDMsg (SD.root <| .currentDeck m) ]
    ]

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
