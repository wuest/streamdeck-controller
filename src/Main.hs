module Main where

import qualified Opts
import qualified Data.ByteString as BS
import qualified Streamdeck      as SD

import Prelude

solidBlue :: Integer -> BS.ByteString
solidBlue 1 = BS.pack $ take (3 * 2583) (cycle [0xff, 0x00, 0x00])
solidBlue 2 = BS.pack $ take (3 * 2601) (cycle [0xff, 0x00, 0xff])
solidBlue _ = BS.pack []

main :: IO ()
main = do
    _     <- Opts.getOpts
    decks <- SD.enumerateStreamDecks
    deck  <- SD.openStreamDeck $ decks !! 0
    _ <- SD.send deck SD.initializationReport
    _ <- SD.writePage deck 1 0 $ solidBlue 1
    _ <- SD.writePage deck 2 0 $ solidBlue 2
    putStrLn $ decks >>= show
