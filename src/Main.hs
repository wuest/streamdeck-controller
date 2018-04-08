module Main where

import qualified Opts
import qualified Streamdeck               as SD
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai              as Wai

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Network.HTTP.Types             as Http

import Prelude

readForever :: SD.Deck -> IO ()
readForever deck = do
    newDeck <- SD.readButtonState deck
    putStrLn $ show $ SD.buttonPressed newDeck 0
    readForever deck


textPlain :: Http.Header
textPlain = (Http.hContentType, BS.pack "text/plain")

static :: String -> Wai.Application
static s request respond = respond $ case Wai.rawPathInfo request of
    _             -> Wai.responseLBS Http.status200 [textPlain] (LBS.fromStrict $ BS.pack s)


--basicApp :: String -> Wai.Application
--basicApp s r f = Wai.responseLBS Http.status200 [(Http.hContentType, BS.pack "text/plain")] 

main :: IO ()
main = do
    _     <- Opts.getOpts
    decks <- SD.enumerateStreamDecks
    deck  <- SD.openStreamDeck $ head decks
    SD.send deck SD.initializationReport
    _ <- SD.updateDeck deck id

    putStrLn $ show $ head decks

    let settings = Warp.setPort 3333 Warp.defaultSettings
    Warp.runSettings settings $ static (show $ head decks)
