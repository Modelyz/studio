{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (
    Chan,
    MVar,
    dupChan,
    forkIO,
    newChan,
    newMVar,
    putMVar,
    readChan,
    takeMVar,
    writeChan,
 )
import Control.Monad (forever, when)
import Control.Monad.Fix (fix)
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS (append)
import Data.Function ((&))
import Data.List ()
import qualified Data.Text as T (Text, append, split, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Event (Event, getString, getUuids, isType, setProcessed)
import qualified EventStore as ES
import Network.HTTP.Types (status200)
import Network.Wai (
    Application,
    pathInfo,
    rawPathInfo,
    responseFile,
    responseLBS,
 )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (
    Connection,
    DataMessage (Binary, Text),
    ServerApp,
    acceptRequest,
    defaultConnectionOptions,
    fromLazyByteString,
    receiveDataMessage,
    sendTextData,
    withPingThread,
 )

type NumClient = Int

type WSState = MVar NumClient

contentType :: T.Text -> T.Text
contentType filename = case reverse $ T.split (== '.') filename of
    "css" : _ -> "css"
    "js" : _ -> "javascript"
    _ -> "raw"

wsApp :: Chan (NumClient, Event) -> WSState -> ServerApp
wsApp chan st pending_conn = do
    chan' <- dupChan chan
    -- accept a new connexion
    conn <- acceptRequest pending_conn
    -- increment the sequence of clients
    nc <- takeMVar st
    putMVar st (nc + 1)
    putStrLn $ "Client " ++ show nc ++ " connecting"
    -- fork a thread to loop on waiting for new messages coming into the chan to send them to the new client
    _ <-
        forkIO $
            fix $
                ( \loop -> do
                    (n, ev) <- readChan chan'
                    when (n /= nc && not (isType "ConnectionInitiated" ev)) $ do
                        putStrLn $ "Read event on channel from client " ++ show n ++ "... sending to client " ++ show nc ++ " through WS"
                        sendTextData conn $ JSON.encode [ev]
                    loop
                )
    -- loop on the handling of messages incoming through websocket
    withPingThread conn 30 (return ()) $
        forever $
            do
                messages <- receiveDataMessage conn
                putStrLn $ "Received string from websocket from client " ++ show nc ++ ". Handling it : " ++ show messages
                case JSON.decode $
                    ( case messages of
                        Text bs _ -> (fromLazyByteString bs)
                        Binary bs -> (fromLazyByteString bs)
                    ) of
                    Just evs -> mapM (handleEvent conn nc chan') evs
                    Nothing -> mapM id [putStrLn $ "Error decoding incoming message"]

handleEvent :: Connection -> NumClient -> Chan (NumClient, Event) -> Event -> IO ()
handleEvent conn nc chan ev =
    do
        -- store the event in the event store
        ES.appendEvent ev
        -- if the event is a ConnectionInitiated, get the uuid list from it,
        -- and send back all the missing events (with an added ack)
        when
            (isType "ConnectionInitiated" ev)
            ( do
                let uuids = getUuids ev
                esevs <- ES.readEvents
                let evs =
                        filter
                            ( \e -> case getString "uuid" e of
                                Just u -> not $ elem (T.unpack u) uuids
                                Nothing -> False
                            )
                            esevs
                sendTextData conn $ JSON.encode evs
                putStrLn $ "Sent all missing " ++ (show $ length evs) ++ " messsages to client " ++ (show nc)
            )
        -- Send back and store an ACK to let the client know the message has been stored
        let ev' = setProcessed ev
        ES.appendEvent ev'
        sendTextData conn $ JSON.encode [ev']
        -- send the msg to other connected clients
        putStrLn $ "Writing to the chan as client " ++ (show nc)
        writeChan chan (nc, ev)
        writeChan chan (nc, ev')

-- TODO send the event back to the central event store so that it be handled by other microservices.

httpApp :: Application
httpApp request respond = do
    rawPathInfo request
        & decodeUtf8
        & T.append "Request "
        & T.unpack
        & putStrLn
    respond $ case pathInfo request of
        "static" : pathtail -> case pathtail of
            filename : _ ->
                let ct = BS.append "text/" (encodeUtf8 (contentType filename))
                 in responseFile status200 [("Content-Type", ct)] ("../build/" ++ T.unpack filename) Nothing
            _ -> responseLBS status200 [("Content-Type", "text/html")] "static directory"
        _ -> responseFile status200 [("Content-Type", "text/html")] ("../build/index.html" :: String) Nothing

main :: IO ()
main = do
    putStrLn "http://localhost:8080/"
    st <- newMVar 0
    chan <- newChan
    run 8080 $ websocketsOr defaultConnectionOptions (wsApp chan st) httpApp
