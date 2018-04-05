module Streamdeck ( Deck (..)
                  , openStreamDeck
                  , enumerateStreamDecks
                  , vendorID, productID
                  , initializationReport
                  , send, Streamdeck.read
                  , writePage
                  ) where

import qualified Data.ByteString as BS
import qualified Data.Word       as DW   (Word16, Word8)
import qualified System.HIDAPI   as HID

import Prelude

data Deck = Deck { ref   :: HID.Device
                 , state :: DeckState
                 }

type DeckState = [Page]

newtype Page = Page (Row, Row, Row)

newtype Row = Row (Item, Item, Item, Item, Item)

data Item = Item { image  :: Image
                 , action :: Action
                 }

type Image = BS.ByteString

data Action = URLAction String
            | ShellAction String
            | NoAction

vendorID :: DW.Word16
vendorID  = 0x0fd9

productID :: DW.Word16
productID = 0x0060

packetSize :: Int
packetSize = 4096

solidRGB :: DW.Word8 -> DW.Word8 -> DW.Word8 -> Image
solidRGB r g b = BS.pack $ take (3 * 72 * 72) $ cycle [b, g, r]

defaultPage :: Page
defaultPage = Page ( Row ( Item { image = solidRGB 255 0 0, action = NoAction }
                         , Item { image = solidRGB 204 0 0, action = NoAction }
                         , Item { image = solidRGB 153 0 0, action = NoAction }
                         , Item { image = solidRGB 102 0 0, action = NoAction }
                         , Item { image = solidRGB  51 0 0, action = NoAction }
                         )
                   , Row ( Item { image = solidRGB 0 255 0, action = NoAction }
                         , Item { image = solidRGB 0 204 0, action = NoAction }
                         , Item { image = solidRGB 0 153 0, action = NoAction }
                         , Item { image = solidRGB 0 102 0, action = NoAction }
                         , Item { image = solidRGB 0  51 0, action = NoAction }
                         )
                   , Row ( Item { image = solidRGB 0 0 255, action = NoAction }
                         , Item { image = solidRGB 0 0 204, action = NoAction }
                         , Item { image = solidRGB 0 0 153, action = NoAction }
                         , Item { image = solidRGB 0 0 102, action = NoAction }
                         , Item { image = solidRGB 0 0  51, action = NoAction }
                         )
                   )

initializationReport :: BS.ByteString
initializationReport = BS.pack [ 0x05                   -- Report 0x05
                               , 0x55, 0xAA, 0xD1, 0x01 -- Command (brightness)
                               , 0x63, 0x00, 0x00, 0x00 -- 0x63 (99%)
                               , 0x00, 0x00, 0x00, 0x00
                               , 0x00, 0x00, 0x00, 0x00
                               ]

send :: Deck -> BS.ByteString -> IO Int
send deck bs = 
    case BS.length bs > packetSize of
        True -> do
            _ <- HID.write (ref deck) $ BS.take packetSize bs
            send deck $ BS.drop packetSize bs
        False -> HID.write (ref deck) bs

writePage :: Deck -> Int -> DW.Word8 -> BS.ByteString -> IO Int
writePage deck p i bs = send deck $ BS.append (page p i) bs

page :: Int -> DW.Word8 -> BS.ByteString
page 1 i = BS.pack [ 0x02, 0x01, 0x01, 0x00, 0x00, (i+1), 0x00, 0x00
                   , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
                   , 0x42, 0x4d, 0xf6, 0x3c, 0x00, 0x00, 0x00, 0x00
                   , 0x00, 0x00, 0x36, 0x00, 0x00, 0x00, 0x28, 0x00
                   , 0x00, 0x00, 0x48, 0x00, 0x00, 0x00, 0x48, 0x00
                   , 0x00, 0x00, 0x01, 0x00, 0x18, 0x00, 0x00, 0x00
                   , 0x00, 0x00, 0xc0, 0x3c, 0x00, 0x00, 0xc4, 0x0e
                   , 0x00, 0x00, 0xc4, 0x0e, 0x00, 0x00, 0x00, 0x00
                   , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]

page 2 i = BS.pack [ 0x02, 0x01, 0x02, 0x00, 0x01, (i+1), 0x00, 0x00
                   , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]

page _ _ = BS.pack []

read :: Deck -> Int -> IO BS.ByteString
read deck c = HID.read (ref deck) c

-- DeviceInfo { path = "/dev/hidraw%d"
--            , vendorId = 4057
--            , produc tId = 96
--            , serialNumber = Just ""
--            , releaseNumber = 256
--            , manufacturerString = Just "Elgato Systems"
--            , productString = Just "Stream Deck"
--            , usagePage = 13410
--            , usage = 13359
--            , interfaceNumber = 0
--            }
enumerateStreamDecks :: IO [HID.DeviceInfo]
enumerateStreamDecks = HID.enumerate (Just vendorID) (Just productID)

openStreamDeck :: HID.DeviceInfo -> IO Deck
openStreamDeck device = HID.withHIDAPI $ do
    deck <- HID.openDeviceInfo device
    return Deck { ref = deck
                , state = [defaultPage]
                }
