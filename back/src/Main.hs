{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO, newChan, Chan, writeChan, readChan, dupChan, MVar, newMVar, takeMVar, putMVar)
import Control.Exception (fromException, SomeException (SomeException), catch)
import Control.Monad (forever, when)
import Control.Monad.Fix (fix)
import Data.Function ((&))
import Data.List ()
import Data.Maybe (Maybe(Nothing))
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Types ( status200, status404 )
import Network.Wai ( responseLBS, Application, Request (requestBody), responseFile, pathInfo, rawPathInfo )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (withPingThread, ServerApp, acceptRequest, sendTextData, defaultConnectionOptions, receiveDataMessage, DataMessage(Text, Binary), send, PendingConnection, Connection)
import System.Posix.Internals (puts)
import Text.JSON ( decode, valFromObj, Result(..), JSValue(JSObject) )
import qualified Data.ByteString as BS (pack, unpack, ByteString, append)
import qualified Data.ByteString.Lazy.Char8 as LBS (unpack)
import qualified Data.Text
import qualified Data.Text as T (unpack, pack, Text, split, append)
import qualified GHC.Num as String
--import Data.ByteString (putStrLn)

eventstorepath :: FilePath
eventstorepath = "eventstore.txt"

contentType :: T.Text -> T.Text
contentType filename = case reverse $ T.split (=='.') filename of
    "css":_ -> "css"
    "js":_ -> "javascript"
    _ -> "raw"


getStringVal :: String -> JSValue -> Result String
getStringVal key (JSObject obj) = valFromObj key obj
getStringVal key _ = Error "Error: JSON message is not an object"

getIntegerVal :: String -> JSValue -> Result Integer
getIntegerVal key (JSObject obj) = valFromObj key obj
getIntegerVal key _ = Error "Error: JSON message is not an object"


skipUntil :: Integer -> [String] -> [String]
skipUntil limit =
    filter (\m -> getIntegerValue "posixtime" m >= limit && getStringValue "type" m /= "ConnectionInitiated")

getIntegerValue :: String -> String -> Integer
getIntegerValue key msg =
    case decode msg >>= getIntegerVal key of
      Ok n -> n
      Error s -> 0

getStringValue :: String -> String -> String
getStringValue key msg =
    case decode msg >>= getStringVal key of
      Ok v -> v
      Error s -> "Error: " ++ s




-- read the event store from the specified date
-- and send all messages to the websocket connection
sendMessagesFrom :: Connection -> Integer -> IO()
sendMessagesFrom conn date = do
    str <- catch (readFile eventstorepath) handleMissing
    let msgs = (unlines . skipUntil date . lines) str
        in do
            sendTextData conn (T.pack msgs :: T.Text)
            putStrLn msgs
    where
        handleMissing :: SomeException -> IO String
        handleMissing (SomeException e) = 
            return ""
    



sendLatestMessages :: Connection ->  String -> Int -> IO()
sendLatestMessages conn msg numClient =
    let lastEventTime = getIntegerValue "lastEventTime" msg
    in do
        case decode msg >>= getStringVal "type" of
            Ok str -> when (str == "ConnectionInitiated") $ sendMessagesFrom conn lastEventTime
            Error str -> putStrLn ("Error: " ++ str)
        print $ "Sent all latest messages to client " ++ (show numClient) ++ " from LastEventTime=" ++ (show lastEventTime)


type Msg = (Int, String)


type WSState = MVar Int


wsApp :: Chan Msg -> WSState -> ServerApp
wsApp chan wsstate pending_conn = do
    chan <- dupChan chan
    -- accept a new connexion
    conn <- acceptRequest pending_conn
    -- increment the sequence of clients
    numClient <- takeMVar wsstate
    putMVar wsstate (numClient + 1)
    print $ "Client " ++ (show numClient) ++ " connecting"
    -- fork a thread to loop on waiting for new messages coming into the chan to send them to the new client
    forkIO $ fix $ (\loop -> do
        (num, msg) <- readChan chan
        when (num/=numClient && (getStringValue "type" msg) /= "ConnectionInitiated")
            $ print $ "Read msg on channel from client " ++ (show num) ++ "... sending to client " ++ (show numClient) ++ " through WS"
        when (num/=numClient && (getStringValue "type" msg) /= "ConnectionInitiated")
            $ sendTextData conn (T.pack msg :: T.Text)
        loop)
    -- loop on the handling of messages incoming through websocket
    withPingThread conn 30 (return ()) $ forever $ do
        message <- receiveDataMessage conn
        print $ "Received string from WS from client " ++ (show numClient) ++ ". Handling it"
        let msg = (case message of
                Text bs _ -> LBS.unpack bs
                Binary bs -> LBS.unpack bs) ++ "\n"
          in do
                -- first store the msg in the event store
                appendFile eventstorepath msg
                -- if the msg is a InitiateConnection, get the lastEventTime from it and send back all the events from that time.
                sendLatestMessages conn msg numClient
                -- send the msg to other connected clients
                print $ "Writing to the chan as client " ++ (show numClient)
                writeChan chan (numClient, msg)
                -- TODO send the msg back to the central event store so that it be handled by other microservices.




httpApp :: Application
httpApp request respond = do
    rawPathInfo request
        & decodeUtf8
        & T.append "Request "
        & T.unpack
        & putStrLn
    respond $ case pathInfo request of
        "static":pathtail -> case pathtail of
                filename:othertail ->
                    let ct = BS.append "text/" (encodeUtf8 (contentType filename))
                    in
                    responseFile
                    status200
                    [("Content-Type", ct)]
                    ("../build/" ++ T.unpack filename)
                    Nothing
                _ -> responseLBS status200 [("Content-Type", "text/html")] "static directory"
        _ -> responseFile status200 [("Content-Type", "text/html")] ("../build/index.html"::String) Nothing


main :: IO ()
main = do
    putStrLn "http://localhost:8080/"
    wsstate <- newMVar 0
    chan <- newChan
    run 8080 $ websocketsOr defaultConnectionOptions (wsApp chan wsstate) httpApp
