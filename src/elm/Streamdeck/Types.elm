module Streamdeck.Types exposing (..)

type Msg = Update String

type alias Model = { activeDeck : Streamdeck
                   , activePage : Int
                   }

type Page = List Button

type alias Button = { imageUrl : String
                    , action : String
                    }

type alias Streamdeck = { name : String
                        , serial : String
                        , pages : List Page
                        }
