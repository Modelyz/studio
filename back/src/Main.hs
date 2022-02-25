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
import Control.Exception (SomeException (SomeException), catch)
import Control.Monad (forever, when)
import Control.Monad.Fix (fix)
import qualified Data.ByteString as BS (append)
import Data.Function ((&))
import Data.List ()
import qualified Data.Text as T (Text, append, intercalate, lines, pack, split, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Event (Event, getIntegerValue, getStringVal, getStringValue, skipUntil)
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
import Text.JSON (Result (..), decode)

type NumClient = Int

type Msg = (NumClient, Event)

type WSState = MVar NumClient

es :: FilePath
es = "eventstore.txt"

contentType :: T.Text -> T.Text
contentType filename = case reverse $ T.split (== '.') filename of
  "css" : _ -> "css"
  "js" : _ -> "javascript"
  _ -> "raw"

-- read the event store from the specified date
-- and send all messages to the websocket connection
sendMessagesFrom :: Connection -> NumClient -> IO ()
sendMessagesFrom conn date = do
  str <- catch (readFile es) handleMissing
  let evs = ((T.intercalate "\n") . skipUntil date . T.lines) (T.pack str)
   in do
        sendTextData conn evs
  where
    handleMissing :: SomeException -> IO String
    handleMissing (SomeException _) =
      return ""

sendLatestMessages :: Connection -> Event -> NumClient -> IO ()
sendLatestMessages conn ev nc =
  let t = getIntegerValue "lastEventTime" ev
   in do
        case decode (T.unpack ev) >>= getStringVal "type" of
          Ok ty -> when (ty == "ConnectionInitiated") $ sendMessagesFrom conn t
          Error e -> putStrLn ("Error: " ++ e)
        print $ "Sent all latest messages to client " ++ (show nc) ++ " from LastEventTime=" ++ (show t)

wsApp :: Chan Msg -> WSState -> ServerApp
wsApp chan st pending_conn = do
  chan' <- dupChan chan
  -- accept a new connexion
  conn <- acceptRequest pending_conn
  -- increment the sequence of clients
  nc <- takeMVar st
  putMVar st (nc + 1)
  print $ "Client " ++ show nc ++ " connecting"
  -- fork a thread to loop on waiting for new messages coming into the chan to send them to the new client
  _ <-
    forkIO $
      fix $
        ( \loop -> do
            (n, ev) <- readChan chan'
            when (n /= nc && getStringValue "type" ev /= "ConnectionInitiated") $
              print $ "Read event on channel from client " ++ show n ++ "... sending to client " ++ show nc ++ " through WS"
            when (n /= nc && getStringValue "type" ev /= "ConnectionInitiated") $
              sendTextData conn ev
            loop
        )
  -- loop on the handling of messages incoming through websocket
  withPingThread conn 30 (return ()) $
    forever $ do
      message <- receiveDataMessage conn
      print $ "Received string from WS from client " ++ show nc ++ ". Handling it : " ++ show message
      let ev =
            ( case message of
                Text bs _ -> (fromLazyByteString bs :: T.Text)
                Binary bs -> (fromLazyByteString bs :: T.Text)
            )
              `T.append` "\n"
       in do
            -- first store the event in the event store
            appendFile es $ T.unpack ev
            -- if the event is a InitiateConnection, get the lastEventTime from it and send back all the events from that time.
            sendLatestMessages conn ev nc
            -- send the msg to other connected clients
            print $ "Writing to the chan as client " ++ (show nc)
            writeChan chan' (nc, ev)

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
