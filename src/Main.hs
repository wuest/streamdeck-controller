{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude
import Control.Monad ( unless )
import Control.Monad.Reader ( runReaderT )

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS

import Web.Scotty.Trans
import Network.Wai.Middleware.Static ( addBase, hasPrefix, noDots, staticPolicy, (>->) )
import Network.Wai.Handler.WebSockets ( websocketsOr )
import Network.WebSockets ( defaultConnectionOptions )

import qualified System.Directory as Dir

import qualified Opts
import qualified Routes
import qualified Websockets as WS
import qualified Streamdeck as SD

readForever :: SD.Deck -> IO ()
readForever deck = do
    newDeck <- SD.readButtonState deck
    putStrLn $ show $ SD.buttonPressed newDeck 0
    readForever deck

start :: Int -> FilePath -> IO ()
start port base = do
    appState <- WS.initState
    scottyT port (`runReaderT` base) $ do
        middleware $ websocketsOr defaultConnectionOptions $ WS.app appState
        middleware $ staticPolicy $ noDots >-> hasPrefix "static" >-> addBase base
        Routes.routes

setupDataDir :: FilePath -> IO ()
setupDataDir path = do
    exists <- Dir.doesDirectoryExist path
    unless exists $ do
        putStrLn $ "Data directory (" ++ path ++ ") doesn't exist - creating..."
        Dir.createDirectory path

    exists' <- Dir.doesDirectoryExist (path ++ "/static")
    unless exists' $ do
        putStrLn $ "Data directory (" ++ path ++ "/static) doesn't exist - creating..."
        Dir.createDirectory (path ++ "/static")

main :: IO ()
main = do
    opts <- Opts.getOpts

---

    decks <- SD.enumerateStreamDecks
    deck  <- SD.openStreamDeck $ head decks
    SD.send deck SD.initializationReport
    _ <- SD.updateDeck deck id

    putStrLn $ show $ head decks

---

    let port = Opts.webPort opts
    base <- Opts.baseDir opts
    _ <- setupDataDir base
    putStrLn $ "Starting service on port " ++ show port
    start port base

---    Warp.runSettings settings $ static (show $ head decks)
