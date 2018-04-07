module Streamdeck ( Deck (..)
                  , openStreamDeck
                  , enumerateStreamDecks
                  , vendorID, productID
                  , initializationReport
                  , send, Streamdeck.read
                  , writePage
                  , writeImage
                  , solidRGB
                  , updateDeck
                  , readButtonState, buttonPressed
                  ) where

import qualified Data.Bits       as B
import qualified Data.ByteString as BS
import qualified Data.Word       as DW   (Word16, Word8)
import qualified System.HIDAPI   as HID

import Prelude

data Deck = Deck { ref       :: HID.Device
                 , state     :: DeckState
                 , activeMap :: ActiveMap
                 }

newtype ActiveMap = ActiveMap [Bool]

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

page1Pixels :: Int
page1Pixels = 2583

page2Pixels :: Int
page2Pixels = 2601

buttonPixels :: Int
buttonPixels = page1Pixels + page2Pixels

solidRGB :: DW.Word8 -> DW.Word8 -> DW.Word8 -> Image
solidRGB r g b = BS.pack $ take (3 * (buttonPixels - 1)) $ cycle [b, g, r]

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

send :: Deck -> BS.ByteString -> IO ()
send deck bs =
    if BS.length bs > packetSize then
        (do _ <- HID.write (ref deck) $ BS.take packetSize bs
            send deck $ fixContinuationPacket bs)
    else
        (do _ <- HID.write (ref deck) bs
            return ())

-- In cases where the first byte of a continuation packet is unset, the byte is
-- discarded, resulting in discoloration
fixContinuationPacket :: BS.ByteString -> BS.ByteString
fixContinuationPacket b
    | BS.length b < packetSize = BS.pack []
    | otherwise =
        let rest = BS.drop packetSize b
            byte = BS.head rest
        in if byte > 0 then rest
                       else BS.cons ((B..|.) 1 byte) (BS.drop 1 rest)

writePage :: Deck -> Int -> DW.Word8 -> BS.ByteString -> IO ()
writePage deck p i bs = send deck $ BS.append (page p i) bs

page :: Int -> DW.Word8 -> BS.ByteString
page 1 i = BS.pack [ 0x02, 0x01, 0x01, 0x00, 0x00,  i+1, 0x00, 0x00
                   , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
                   , 0x42, 0x4d, 0xf6, 0x3c, 0x00, 0x00, 0x00, 0x00
                   , 0x00, 0x00, 0x36, 0x00, 0x00, 0x00, 0x28, 0x00
                   , 0x00, 0x00, 0x48, 0x00, 0x00, 0x00, 0x48, 0x00
                   , 0x00, 0x00, 0x01, 0x00, 0x18, 0x00, 0x00, 0x00
                   , 0x00, 0x00, 0xc0, 0x3c, 0x00, 0x00, 0xc4, 0x0e
                   , 0x00, 0x00, 0xc4, 0x0e, 0x00, 0x00, 0x00, 0x00
                   , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]

page 2 i = BS.pack [ 0x02, 0x01, 0x02, 0x00, 0x01,  i+1, 0x00, 0x00
                   , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]

page _ _ = BS.pack []

read :: Deck -> Int -> IO BS.ByteString
read deck = HID.read (ref deck)

emptyActiveMap :: ActiveMap
emptyActiveMap = ActiveMap $ replicate 15 False

-- Stream Deck reports button state ONLY upon button press/release
-- Stream Deck will send a 16 byte message, with the following format:
-- 01 AA BB CC DD EE FF GG HH II JJ KK LL MM NN OO
-- * Byte 0 being set to 0x01 is static, indicating a "button event" message.
-- * AA-OO are 1 byte, if the low bit is set, the button is pressed.  Bits 1-7
--   appear to be unused.
readButtonState :: Deck -> IO Deck
readButtonState deck = do
    currentState <- Streamdeck.read deck 16
    return deck { activeMap = bytesToActiveMap $ BS.unpack currentState }

bytesToActiveMap :: [DW.Word8] -> ActiveMap
bytesToActiveMap xs
    | length xs /= 16 = emptyActiveMap
    | otherwise = ActiveMap $ map (== 1) $ drop 1 xs

buttonPressed :: Deck -> Int -> Bool
buttonPressed deck = buttonPressed' (activeMap deck)

buttonPressed' :: ActiveMap -> Int -> Bool
buttonPressed' (ActiveMap a) i = a !! i

writeImage :: Deck -> DW.Word8 -> Image -> IO ()
writeImage deck button img =
    let page1 = BS.take (3 * page1Pixels) img
        page2 = BS.take (3 * page2Pixels) $ BS.drop (3 * page1Pixels) img
    in do
        writePage deck 1 button page1
        writePage deck 2 button page2

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
                , activeMap = emptyActiveMap
                }

drawRow :: Deck -> DW.Word8 -> Row -> IO ()
drawRow d r (Row (i0, i1, i2, i3, i4)) = do
    writeImage d (r * 5) $ image i0
    writeImage d (r * 5 + 1) $ image i1
    writeImage d (r * 5 + 2) $ image i2
    writeImage d (r * 5 + 3) $ image i3
    writeImage d (r * 5 + 4) $ image i4

drawPage :: Deck -> Page -> IO ()
drawPage d (Page (r0, r1, r2)) = do
    drawRow d 0 r0
    drawRow d 1 r1
    drawRow d 2 r2

-- TODO
updateDeck :: Deck -> (DeckState -> DeckState) -> IO Deck
updateDeck d _ = do
    drawPage d $ head $ state d
    return d
