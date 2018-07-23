module Decklist.Types exposing (..)

type Msg = Conn Decklist

type alias Decklist = List Deckinfo

type alias Deckinfo = { name : String
                      , serial : String
                      }
