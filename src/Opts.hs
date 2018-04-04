{-# LANGUAGE OverloadedStrings #-}

module Opts ( Options (..)
            , getOpts
            ) where
 
import Prelude
import System.Console.GetOpt

import qualified Streamdeck         as SD  (vendorID, productID)
import qualified Data.Word          as DW  (Word16)
import qualified System.Environment as Env (getProgName, getArgs)
import qualified System.Exit        as Sys (exitSuccess)
import qualified System.IO          as IO  (hPutStrLn, stderr)
import qualified Text.Printf        as P   (printf)

newtype Options = Options { optVerbose  :: Bool -- Verbosity
                          }

showUDev :: Options -> IO Options
showUDev _ = do
    IO.hPutStrLn IO.stderr $ unlines
        [ "# Global access UDEV file for Elgato Stream Deck Generated by streamdeck-controller"
        , "#"
        , "# Install to /etc/udev/rules.d/elgato.rules to assign rw-rw-rw- permissions to"
        , "# all Stream Deck devices."
        , "#"
        , "# See https://github.com/wuest/streamdeck-controller/blob/master/README.md for more"
        , "# information."
        , ""
        , line "SUBSYSTEM" "usb" SD.vendorID SD.productID
        , line "KERNEL" "hidraw*" SD.vendorID SD.productID
        ]
    Sys.exitSuccess
  where
    line :: String -> String -> DW.Word16 -> DW.Word16 -> String
    line = P.printf "%s==\"%s\", ATTRS{idVendor}==\"%04x\", ATTRS{idProduct}==\"%04x\", MODE=\"0666\""

defaultOptions :: Options
defaultOptions = Options { optVerbose  = False
                         }

version :: String
version = "0.0.1"

printVersion :: Options -> IO Options
printVersion _ = do
    prg <- Env.getProgName
    IO.hPutStrLn IO.stderr $ prg ++ " version " ++ version
    Sys.exitSuccess

printHelp :: Options -> IO Options
printHelp _ = do
    prg <- Env.getProgName
    IO.hPutStrLn IO.stderr (usageInfo prg options)
    Sys.exitSuccess

verbose :: Options -> IO Options
verbose opt = return opt { optVerbose = True }

--Used eventually
--blank :: OptDescr (Options -> IO Options)
--blank = Option [] [] (NoArg return) ""

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option ['d'] ["dbus"]
        (NoArg showUDev) "Generate udev configuration file for globally accessible Stream Decks"

    , Option ['v'] ["verbose"]
        (NoArg verbose) "Enable verbose messages (currently does nothing)"

    , Option ['V'] ["version"]
        (NoArg printVersion) "Print version"

    , Option ['h', '?'] ["help"]
        (NoArg printHelp) "Show help"
    ]

getOpts :: IO Options
getOpts = do
    args <- Env.getArgs
    let (actions, _nonoptions, _errors) = getOpt RequireOrder options args
    foldl (>>=) (return defaultOptions) actions
