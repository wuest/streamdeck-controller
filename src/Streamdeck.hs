module Streamdeck ( enumerateStreamDecks
                  , vendorID, productID
                  ) where

import qualified Data.Word      as DW   (Word16)
import qualified System.HIDAPI  as HID

import Prelude

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
enumerateStreamDecks :: IO String
enumerateStreamDecks = do
    decks <- HID.enumerate (Just vendorID) (Just productID)
    return $ decks >>= show
