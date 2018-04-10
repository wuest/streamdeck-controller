module Main where

import qualified Opts
import qualified Streamdeck                     as SD
import qualified Control.Concurrent             as Concurrent

import qualified Configurator.Web               as Web

import Prelude

readForever :: SD.Deck -> IO ()
readForever deck = do
    newDeck <- SD.readButtonState deck
    print $ SD.buttonPressed newDeck 0
    readForever deck

main :: IO ()
main = do
    opts  <- Opts.getOpts
    decks <- SD.enumerateStreamDecks
    deck  <- SD.openStreamDeck $ head decks
    SD.send deck SD.initializationReport
    _ <- SD.updateDeck deck id

    print $ head decks

    -- TODO: Predicate this on configurator being enabled
    clients <- Concurrent.newMVar []
    Web.start clients 3333
