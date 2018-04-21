module Decklist.Types exposing (..)

type Msg = String

type alias Model = { deckList : Decklist
                   }

type alias Decklist = List Deckinfo

type alias Deckinfo = { name : String
                      , serial : String
                      }
