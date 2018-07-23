module Main exposing (main)

import Html            as H
import Html.Events     as Event
import Html.Attributes as Attr
import WebSocket       as WS
import Json.Decode     as Json

import Streamdeck.View  as SD
import Streamdeck.Types as SD

import Decklist.View  as DL
import Decklist.Types as DL
import Decklist.Comm  as DL

type alias Model = { deckList : DL.Decklist
                   , activeDeck : Maybe SD.Streamdeck
                   , sockUrl : String
                   }

type alias Flags = { webPort : String }

type Msg = Receive String
         | SDMsg SD.Msg
         | DLMsg DL.Msg

decodeDecklist : String -> Result String DL.Decklist
decodeDecklist = Json.decodeString DL.decklistFrom

decksFrom : String -> DL.Decklist
decksFrom msg =
    let result = decodeDecklist msg in
        case result of
            Err _ -> []
            Ok  a -> a

main : Program Flags Model Msg
main = H.programWithFlags
    { init          = init
    , update        = update
    , view          = view
    , subscriptions = subscriptions
    }

init : Flags -> (Model, Cmd Msg)
init flags = { activeDeck = Nothing
             , deckList = []
             , sockUrl = "ws://localhost:" ++ .webPort flags
             } ! []

update : Msg -> Model -> (Model, Cmd Msg)
update msg m = case msg of
    Receive s -> { m | deckList = decksFrom s } ! []
    SDMsg (SD.Update _) -> m ! []
    DLMsg (DL.Conn l) -> { m | deckList = l } ! []

view : Model -> H.Html Msg
view m =
  H.div []
    [ H.div []
      [ H.map DLMsg (DL.root <| .deckList m) ]
    , H.div []
      [ H.map SDMsg (SD.root <| .activeDeck m) ]
    ]

subscriptions : Model -> Sub Msg
subscriptions m = Sub.batch [ WS.listen (.sockUrl m) Receive
                            ]
