module Streamdeck ( Deck (..)
                  , enumerateStreamDecks
                  , vendorID, productID
                  ) where

import qualified Data.ByteString as BS
import qualified Data.Word       as DW   (Word16)
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

vendorID :: DW.Word16
vendorID  = 0x0fd9

productID :: DW.Word16
productID = 0x0060

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
                , state = []
                }
