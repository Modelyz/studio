{-# LANGUAGE OverloadedStrings #-}

import Network.Wai ( responseLBS, Application, Request (requestBody), responseFile, pathInfo, rawPathInfo )
import Network.HTTP.Types ( status200, status404 )
import Network.Wai.Handler.Warp (run)
import Data.Maybe (Maybe(Nothing))
import qualified Data.Text as T (unpack, pack, Text, split, append)
import Data.List ()
import qualified Data.ByteString as BS (pack, ByteString, append)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Function ((&))
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp, acceptRequest, sendTextData, defaultConnectionOptions)

contentType :: T.Text -> T.Text
contentType filename = case reverse $ T.split (=='.') filename of
    "css":_ -> "css"
    "js":_ -> "javascript"
    _ -> "raw"


wsApp :: ServerApp
wsApp pending_conn = do
        conn <- acceptRequest pending_conn
        sendTextData conn ("Hello, client!" :: T.Text)


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
