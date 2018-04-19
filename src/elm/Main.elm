module Main exposing (main)

import Html            as H
import Html.Events     as Event
import Html.Attributes as Attr

type Msg = String

type alias Model = {}

type alias Flags = {}

main : Program Flags Model Msg
main = H.programWithFlags
    { init          = init
    , update        = update
    , view          = view
    , subscriptions = subscriptions
    }

init : Flags -> (Model, Cmd Msg)
init _ = { } ! []

update : Msg -> Model -> (Model, Cmd Msg)
update _ _ = { } ! []

view : Model -> H.Html Msg
view _ =
  H.div []
    [ H.select []
      (List.map (\n -> H.option [ Attr.value (toString n) ] [ H.text (toString n) ]) (List.range 1 12))
    ]

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
