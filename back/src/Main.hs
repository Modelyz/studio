{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
  ( Chan,
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
import qualified Data.ByteString as BS (append)
import Data.Function ((&))
import Data.List ()
import qualified Data.Text as T (Text, append, pack, split, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.UUID.V4 (nextRandom)
import Event (Event, ack, excludeType, getInt, getString, isAfter, isType)
import qualified EventStore as ES
import Network.HTTP.Types (status200)
import Network.Wai
  ( Application,
    pathInfo,
    rawPathInfo,
    responseFile,
    responseLBS,
  )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets
  ( Connection,
    DataMessage (Binary, Text),
    ServerApp,
    acceptRequest,
    defaultConnectionOptions,
    fromLazyByteString,
    receiveDataMessage,
    sendTextData,
    withPingThread,
  )
import Text.JSON as JSON (Result (..), decode, encode)

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
              sendTextData conn $ T.pack $ encode [ev]
            loop
        )
  -- loop on the handling of messages incoming through websocket
  withPingThread conn 30 (return ()) $
    forever $
      do
        messages <- receiveDataMessage conn
        putStrLn $ "Received string from websocket from client " ++ show nc ++ ". Handling it : " ++ show messages
        let events =
              JSON.decode $
                T.unpack $
                  ( case messages of
                      Text bs _ -> (fromLazyByteString bs :: T.Text)
                      Binary bs -> (fromLazyByteString bs :: T.Text)
                  )
         in case events of
              Ok evs -> mapM (handleEvent conn nc chan') evs
              Error e -> mapM id [putStrLn $ "Error decoding incoming message: " ++ e]

handleEvent :: Connection -> NumClient -> Chan (NumClient, Event) -> Event -> IO ()
handleEvent conn nc chan ev =
  do
    -- first store the event in the event store
    ES.appendEvent ev
    -- Send back an ACK to let the client the message has been stored
    posixtime <- getPOSIXTime
    uuid <- nextRandom
    case getString "uuid" ev of
      Ok origin -> do
        let a = ack uuid (floor $ posixtime * 1000) origin
            as = [a]
        sendTextData conn $ T.pack $ encode as
        ES.appendEvent a
        putStrLn $ "Sent ACK to client " ++ show nc ++ " : " ++ encode a
      Error _ -> return ()
    -- if the event is a InitiateConnection, get the lastEventTime from it
    -- and send back all the events from that time (with an ack)
    when
      (isType "ConnectionInitiated" ev)
      ( case getInt "lastEventTime" ev of
          Ok time -> do
            evs <- fmap (excludeType "ConnectionInitiated" . filter (isAfter time)) ES.readEvents
            sendTextData conn $ T.pack $ encode evs
          Error _ -> return ()
      )
    -- send the msg to other connected clients
    putStrLn $ "Writing to the chan as client " ++ (show nc)
    writeChan chan (nc, ev)

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
