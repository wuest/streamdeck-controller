module Main where

import Control.Monad ( when, forever )

import qualified System.Hardware.Streamdeck as SD
import qualified Control.Concurrent         as Concurrent
import qualified Control.Concurrent.STM     as STM
import qualified Data.ByteString            as BS
import qualified Data.Vector.Unboxed        as Vector
import qualified Codec.Picture.Repa         as Rep
import qualified System.HIDAPI              as HID (DeviceInfo)

import qualified Opts
import qualified Configurator.Web           as Web

import Prelude

-- TODO : duplicated type.  Fix this.
type DeckList = [HID.DeviceInfo]

readImg :: IO BS.ByteString
readImg = do
    pic <- Rep.readImageRGB "GoldW.png"
    return $ case pic of
        Right p -> BS.pack $ Vector.toList $ Rep.toUnboxed $ Rep.reverseColorChannel p

decklistChanged :: DeckList -> DeckList -> Bool
decklistChanged xs ys = map SD.serialNumber xs /= map SD.serialNumber ys

watchDecks :: STM.TChan DeckList -> DeckList -> IO ()
watchDecks chan decks = do
    Concurrent.threadDelay $ seconds 1
    newDecks <- SD.enumerateStreamDecks
    when (decklistChanged decks newDecks) $
        STM.atomically $ STM.writeTChan chan newDecks
    watchDecks chan newDecks
  where
    seconds = (*1000000)

temporaryPrinter :: STM.TChan DeckList -> IO ()
temporaryPrinter broadcastChan = do
    chan <- STM.atomically $ STM.dupTChan broadcastChan
    forever $ do
        ds <- STM.atomically $ STM.readTChan chan
        print ds

main :: IO ()
main = do
    opts  <- Opts.getOpts
    chan <- STM.atomically STM.newBroadcastTChan
    _ <- Concurrent.forkIO $ watchDecks chan []
    _ <- Concurrent.forkIO $ temporaryPrinter chan
-- i <- readImg
--   deck  <- SD.openStreamDeck $ head decks
--   SD.sendRaw deck $ SD.setBrightness 99
--   _ <- SD.updateDeck deck id
 --   SD.writeImage deck 7 i

    when (Opts.optWeb opts) $
        let port = Opts.webPort opts in do
            putStrLn $ ("Configurator running on port " ++) $ show port
            clients <- Concurrent.newMVar []
            Web.start clients chan port
