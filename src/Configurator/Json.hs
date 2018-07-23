{-# LANGUAGE DeriveGeneric #-}

-- | Provides Json types for communication with the Elm frontend
module Configurator.Json where

import Prelude
import Data.Text    ( Text )
import Data.Aeson   ( FromJSON, ToJSON )
import GHC.Generics ( Generic )

data Deck = Deck
    { name   :: Text
    , serial :: Text
    } deriving ( Show, Generic )
instance FromJSON Deck
instance ToJSON Deck

newtype DeckList = DeckList
    { decks :: [Deck]
    } deriving ( Show, Generic )
instance FromJSON DeckList
instance ToJSON DeckList
