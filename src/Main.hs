module Main where

import qualified Opts       as Opts
import qualified Streamdeck as SD

import Prelude

main :: IO ()
main = do
    decks <- SD.enumerateStreamDecks
    options <- Opts.getOpts
    return decks >>= putStrLn
