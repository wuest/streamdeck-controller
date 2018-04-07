module Main where

import qualified Opts
import qualified Streamdeck as SD

import Prelude

readForever :: SD.Deck -> IO ()
readForever deck = do
    newDeck <- SD.readButtonState deck
    putStrLn $ show $ SD.buttonPressed newDeck 0
    readForever deck

main :: IO ()
main = do
    _     <- Opts.getOpts
    decks <- SD.enumerateStreamDecks
    deck  <- SD.openStreamDeck $ head decks
    SD.send deck SD.initializationReport
    _ <- SD.updateDeck deck id

    readForever deck

    putStrLn $ decks >>= show
