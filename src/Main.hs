module Main where

import qualified Opts
import qualified Streamdeck as SD

import Prelude

main :: IO ()
main = do
    _     <- Opts.getOpts
    decks <- SD.enumerateStreamDecks
    deck  <- SD.openStreamDeck $ head decks
    SD.send deck SD.initializationReport
    SD.updateDeck deck id
    putStrLn $ decks >>= show
