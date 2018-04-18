module Main where

import qualified Opts
import qualified System.Hardware.Streamdeck     as SD
import qualified Control.Concurrent             as Concurrent

import qualified Configurator.Web               as Web

import qualified Data.ByteString                as BS
import qualified Data.Vector.Unboxed            as Vector
import qualified Codec.Picture.Repa             as Rep

import Prelude

readImg = do
    pic <- Rep.readImageRGB "GoldW.png"
    return $ case pic of
        Right p -> BS.pack $ Vector.toList $ Rep.toUnboxed $ Rep.reverseColorChannel p

main :: IO ()
main = do
    opts  <- Opts.getOpts
    decks <- SD.enumerateStreamDecks
    deck  <- SD.openStreamDeck $ head decks
    SD.sendRaw deck $ SD.setBrightness 99
    _ <- SD.updateDeck deck id

    print $ head decks
    i <- readImg
    SD.writeImage deck 7 i

    -- TODO: Predicate this on configurator being enabled
    clients <- Concurrent.newMVar []
    Web.start clients 3333
