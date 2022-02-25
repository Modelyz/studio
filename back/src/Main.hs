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
import qualified Data.Text as T (Text, append, intercalate, pack, split, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Event (getIntegerValue, getStringVal, getStringValue, skipUntil)
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
import Text.JSON (JSValue (JSObject), Result (..), decode, valFromObj)

type NumClient = Int

type Msg = (Int, String)

type WSState = MVar Int

eventstorepath :: FilePath
eventstorepath = "eventstore.txt"

contentType :: T.Text -> T.Text
contentType filename = case reverse $ T.split (== '.') filename of
  "css" : _ -> "css"
  "js" : _ -> "javascript"
  _ -> "raw"

-- read the event store from the specified date
-- and send all messages to the websocket connection
sendMessagesFrom :: Connection -> Int -> IO ()
sendMessagesFrom conn date = do
  str <- catch (readFile eventstorepath) handleMissing
  let msgs = ((T.intercalate "\n") . (fmap T.pack) . skipUntil date . lines) str
   in do
        sendTextData conn msgs
  where
    handleMissing :: SomeException -> IO String
    handleMissing (SomeException _) =
      return ""

sendLatestMessages :: Connection -> String -> Int -> IO ()
sendLatestMessages conn msg numClient =
  let lastEventTime = getIntegerValue "lastEventTime" msg
   in do
        case decode msg >>= getStringVal "type" of
          Ok str -> when (str == "ConnectionInitiated") $ sendMessagesFrom conn lastEventTime
          Error str -> putStrLn ("Error: " ++ str)
        print $ "Sent all latest messages to client " ++ (show numClient) ++ " from LastEventTime=" ++ (show lastEventTime)

wsApp :: Chan Msg -> WSState -> ServerApp
wsApp chan wsstate pending_conn = do
  chan' <- dupChan chan
  -- accept a new connexion
  conn <- acceptRequest pending_conn
  -- increment the sequence of clients
  numClient <- takeMVar wsstate
  putMVar wsstate (numClient + 1)
  print $ "Client " ++ show numClient ++ " connecting"
  -- fork a thread to loop on waiting for new messages coming into the chan to send them to the new client
  _ <-
    forkIO $
      fix $
        ( \loop -> do
            (num, msg) <- readChan chan'
            when (num /= numClient && getStringValue "type" msg /= "ConnectionInitiated") $
              print $ "Read msg on channel from client " ++ show num ++ "... sending to client " ++ show numClient ++ " through WS"
            when (num /= numClient && getStringValue "type" msg /= "ConnectionInitiated") $
              sendTextData conn (T.pack msg :: T.Text)
            loop
        )
  -- loop on the handling of messages incoming through websocket
  withPingThread conn 30 (return ()) $
    forever $ do
      message <- receiveDataMessage conn
      print $ "Received string from WS from client " ++ show numClient ++ ". Handling it : " ++ show message
      let msg =
            ( case message of
                Text bs _ -> (fromLazyByteString bs :: T.Text)
                Binary bs -> (fromLazyByteString bs :: T.Text)
            )
              `T.append` "\n"
       in do
            -- first store the msg in the event store
            appendFile eventstorepath $ T.unpack msg
            -- if the msg is a InitiateConnection, get the lastEventTime from it and send back all the events from that time.
            sendLatestMessages conn (T.unpack msg) numClient
            -- send the msg to other connected clients
            print $ "Writing to the chan as client " ++ (show numClient)
            writeChan chan' (numClient, T.unpack msg)

-- TODO send the msg back to the central event store so that it be handled by other microservices.

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
  wsstate <- newMVar 0
  chan <- newChan
  run 8080 $ websocketsOr defaultConnectionOptions (wsApp chan wsstate) httpApp
