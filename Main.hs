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
import Network.WebSockets (ServerApp, acceptRequest, sendTextData, defaultConnectionOptions, receiveDataMessage, DataMessage(Text, Binary), send)
import Control.Monad (forever)
import Text.JSON
import qualified Data.Text
import GHC.Generics
import System.Posix.Internals (puts)



contentType :: T.Text -> T.Text
contentType filename = case reverse $ T.split (=='.') filename of
    "css":_ -> "css"
    "js":_ -> "javascript"
    _ -> "raw"


handleMessage :: String -> IO ()
handleMessage msg = do
    sendLatestMessages msg
    -- first store the msg in the event store
    appendFile "eventstore.txt" msg
    -- then if the msg is a InitiateConnection, get the lastEventTime from it and send back all the events from that time.
    -- otherwise, send the msg back to the central event store so that it be handled by other microservices.




getValue :: String -> JSValue -> Result String
getValue key (JSObject obj) = valFromObj key obj
getValue key _ = Error ""


sendLatestMessages :: String -> IO()
sendLatestMessages msg = do
    let val = do
            jsv <- decode msg
            getValue "type" jsv
        in case val of
            Ok str -> putStrLn str
            Error str -> putStrLn ("Error" ++ str)


wsApp :: ServerApp
wsApp pending_conn = do
        conn <- acceptRequest pending_conn
        sendTextData conn ("Hello, client!" :: T.Text)
        forever $ do
            msg <- receiveDataMessage conn
            let message = (case msg of
                    Text bs (Just text) -> LBS.unpack bs
                    Text bs Nothing -> LBS.unpack bs
                    Binary bs -> LBS.unpack bs) ++ "\n"
                in handleMessage message



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
                    ("../pwa/src/static/" ++ T.unpack filename)
                    Nothing
                _ -> responseLBS status200 [("Content-Type", "text/html")] "static directory"
        _ -> responseFile status200 [("Content-Type", "text/html")] ("../pwa/src/templates/index.html"::String) Nothing


app :: Application
app = websocketsOr defaultConnectionOptions wsApp httpApp


main :: IO ()
main = do
    putStrLn "http://localhost:8080/"
    run 8080 app
