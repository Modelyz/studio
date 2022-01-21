{-# LANGUAGE OverloadedStrings #-}

import Network.Wai ( responseLBS, Application, Request (requestBody), responseFile, pathInfo, rawPathInfo )
import Network.HTTP.Types ( status200, status404 )
import Network.Wai.Handler.Warp (run)
import Data.Maybe (Maybe(Nothing))
import qualified Data.Text as T (unpack, pack, Text, split, append)
import Data.List ()
import qualified Data.ByteString as BS (pack, unpack, ByteString, append)
import qualified Data.ByteString.Lazy.Char8 as LBS (unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Function ((&))
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp, acceptRequest, sendTextData, defaultConnectionOptions, receiveDataMessage, DataMessage(Text, Binary), send, Connection)
import Control.Monad (forever, when)
import Text.JSON
    ( decode, valFromObj, Result(..), JSValue(JSObject) )
import qualified Data.Text
import System.Posix.Internals (puts)
import qualified GHC.Num as String

eventstorepath :: FilePath
eventstorepath = "eventstore.txt"

contentType :: T.Text -> T.Text
contentType filename = case reverse $ T.split (=='.') filename of
    "css":_ -> "css"
    "js":_ -> "javascript"
    _ -> "raw"


handleMessage :: Connection ->  String -> IO ()
handleMessage conn msg = do
    sendLatestMessages conn msg
    -- first store the msg in the event store
    appendFile eventstorepath msg
    -- then if the msg is a InitiateConnection, get the lastEventTime from it and send back all the events from that time.
    -- otherwise, send the msg back to the central event store so that it be handled by other microservices.


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
sendMessagesFrom :: Connection -> Integer -> IO ()
sendMessagesFrom conn date = do
            ms <- fmap (skipUntil date . lines) (readFile eventstorepath)
            sendTextData conn $ T.pack (unlines ms)
            putStrLn $ unlines ms



sendLatestMessages :: Connection ->  String -> IO()
sendLatestMessages conn msg =
    let lastEventTime = getIntegerValue "lastEventTime" msg
    in do
        case decode msg >>= getStringVal "type" of
            Ok str -> when (str == "ConnectionInitiated") $ sendMessagesFrom conn lastEventTime
            Error str -> putStrLn ("Error: " ++ str)
        putStr "LastEventTime="
        print lastEventTime



wsApp :: ServerApp
wsApp pending_conn = do
        conn <- acceptRequest pending_conn
        --sendTextData conn ("Hello, client!" :: T.Text)
        forever $ do
            msg <- receiveDataMessage conn
            let message = (case msg of
                    Text bs (Just text) -> LBS.unpack bs
                    Text bs Nothing -> LBS.unpack bs
                    Binary bs -> LBS.unpack bs) ++ "\n"
                in handleMessage conn message



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
                    ("../front/static/" ++ T.unpack filename)
                    Nothing
                _ -> responseLBS status200 [("Content-Type", "text/html")] "static directory"
        _ -> responseFile status200 [("Content-Type", "text/html")] ("../front/templates/index.html"::String) Nothing


app :: Application
app = websocketsOr defaultConnectionOptions wsApp httpApp


main :: IO ()
main = do
    putStrLn "http://localhost:8080/"
    run 8080 app
