module Main where

import qualified Opts
import qualified Streamdeck as SD

import Prelude

main :: IO ()
main = do
    _     <- Opts.getOpts
    decks <- SD.enumerateStreamDecks
    putStrLn $ decks >>= show
