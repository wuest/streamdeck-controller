module Main where

import qualified Opts
import qualified Streamdeck as SD

import Prelude

main :: IO ()
main = do
    decks <- SD.enumerateStreamDecks
    _     <- Opts.getOpts
    putStrLn decks
