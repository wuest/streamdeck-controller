{-# LANGUAGE OverloadedStrings #-}

module Configurator.Web ( start ) where

import Prelude

import qualified Control.Concurrent             as Concurrent
import qualified Control.Exception              as Exception
import qualified Control.Monad                  as Monad
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Internal       as BS ( c2w )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.List                      as List
import qualified Data.Maybe                     as Maybe
import qualified Data.Text                      as Text

import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.WebSockets             as WS
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.Wai.Handler.Warp       as Warp

import qualified Configurator.Const                   as Const

type ClientId   = Int
type Client     = (ClientId, WS.Connection)
type ClientList = Concurrent.MVar [Client]

textHtml :: Http.Header
textHtml = (Http.hContentType, "text/html")

textJs :: Http.Header
textJs = (Http.hContentType, "text/javascript")

textCss :: Http.Header
textCss = (Http.hContentType, "text/css")

globalJS :: Int -> LBS.ByteString
globalJS port = LBS.fromStrict . BS.pack $ fmap BS.c2w ("webPort = \"" ++ show port ++ "\";\n")

nextId :: [Client] -> ClientId
nextId = Maybe.maybe 0 (1 +) . maxM . List.map fst

maxM :: Ord a => [a] -> Maybe a
maxM [] = Nothing
maxM xs = Just $ maximum xs

connectClient :: WS.Connection -> ClientList -> IO ClientId
connectClient conn stateRef = Concurrent.modifyMVar stateRef $ \state -> do
    let clientId = nextId state
    return ((clientId, conn) : state, clientId)

withoutClient :: ClientId -> [Client] -> [Client]
withoutClient clientId = List.filter ((/=) clientId . fst)

disconnectClient :: ClientId -> ClientList -> IO ()
disconnectClient clientId stateRef = Concurrent.modifyMVar_ stateRef $ \state ->
    return $ withoutClient clientId state


sendFrom :: ClientId -> ClientList -> Text.Text -> IO ()
sendFrom clientId stateRef msg = do
    clients <- Concurrent.readMVar stateRef
    let otherClients = withoutClient clientId clients
    Monad.forM_ otherClients $ \(_, conn) ->
        WS.sendTextData conn msg

-- TODO: handle
startApp :: WS.Connection -> ClientId -> ClientList -> IO ()
startApp conn clientId stateRef = Monad.forever $
    WS.receiveData conn >>= sendFrom clientId stateRef

static :: Int -> Wai.Application
static port request respond = respond $ case Wai.rawPathInfo request of
    "/"           -> Wai.responseLBS Http.status200 [textHtml] Const.mainApp
    "/main.js"    -> Wai.responseLBS Http.status200 [textJs] Const.mainJS
    "/main.css"   -> Wai.responseLBS Http.status200 [textCss] Const.mainCSS
    "/globals.js" -> Wai.responseLBS Http.status200 [textJs] $ globalJS port
    _             -> Wai.responseLBS Http.status404 [textHtml] "Not found"

server :: ClientList -> WS.ServerApp
server stateRef pendingConn = do
    conn <- WS.acceptRequest pendingConn
    clientId <- connectClient conn stateRef
    WS.forkPingThread conn 30
    Exception.finally
        (startApp conn clientId stateRef)
        (disconnectClient clientId stateRef)

start :: ClientList -> Int -> IO ()
start clients port = do
    let settings = Warp.setPort port $ Warp.setHost "127.0.0.1" Warp.defaultSettings
    Warp.runSettings settings $ WS.websocketsOr
        WS.defaultConnectionOptions (server clients) $ static port
