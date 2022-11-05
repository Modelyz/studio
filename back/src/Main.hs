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
import Control.Monad (forever, unless, when)
import Control.Monad.Fix (fix)
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson as JSON (decode, encode)
import qualified Data.ByteString as BS (append)
import Data.Function ((&))
import qualified Data.HashMap.Lazy as HashMap
import Data.List ()
import qualified Data.Text as T (Text, append, split, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as T
import Message (Message, getMetaString, getUuids, isType, setProcessed)
import qualified MessageStore as ES
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
    runClient,
    sendTextData,
    withPingThread,
 )
import qualified Network.WebSockets as WS
import Options.Applicative

-- dir, port, file
data Options = Options !String !Port !FilePath

type NumClient = Int

type Port = Int

type WSState = MVar NumClient

portOption :: Parser Port
portOption =
    option auto (long "port" <> short 'p' <> metavar "PORT" <> value 8080 <> help "Bind socket to this port.  [default: 8080]")

options :: Parser Options
options =
    Options
        <$> strOption (short 'd' <> long "dir" <> value "." <> help "Directory containing the index file and static directory")
        <*> portOption
        <*> strOption (short 'f' <> long "file" <> value "messagestore.txt" <> help "Filename of the file containing messages")

contentType :: T.Text -> T.Text
contentType filename = case reverse $ T.split (== '.') filename of
    "css" : _ -> "css"
    "js" : _ -> "javascript"
    _ -> "raw"

wsApp :: FilePath -> Chan (NumClient, Message) -> WSState -> ServerApp
wsApp f chan st pending_conn = do
    elmChan <- dupChan chan -- channel to the browser application
    -- accept a new connexion
    conn <- acceptRequest pending_conn
    -- increment the sequence of clients
    nc <- takeMVar st
    putMVar st (nc + 1)
    putStrLn $ "Client " ++ show nc ++ " connecting"
    -- fork a thread to loop on waiting for new messages coming into the chan to send them to the new client
    _ <-
        forkIO $
            fix
                ( \loop -> do
                    (n, ev) <- readChan elmChan
                    when (n /= nc && not (isType "InitiatedConnection" ev)) $ do
                        putStrLn $ "Read message on channel from client " ++ show n ++ "... sending to client " ++ show nc ++ " through WS"
                        sendTextData conn $ JSON.encode $ HashMap.singleton ("messages" :: T.Text) [ev]
                    loop
                )

    -- loop on the handling of messages incoming through websocket
    withPingThread conn 30 (return ()) $
        forever $
            do
                messages <- receiveDataMessage conn
                putStrLn $ "Received string from websocket from client " ++ show nc ++ ". Handling it : " ++ show messages
                case JSON.decode
                    ( case messages of
                        Text bs _ -> fromLazyByteString bs
                        Binary bs -> fromLazyByteString bs
                    ) of
                    Just evs -> mapM (handleMessage f conn nc elmChan) evs
                    Nothing -> sequence [putStrLn "Error decoding incoming message"]

clientApp :: Chan (NumClient, Message) -> WS.ClientApp ()
clientApp storeChan conn = do
    putStrLn "Connected!"
    -- TODO: Use the Flow to determine if it has been received by the store, in case the store was not alive.

    -- fork a thread to send back data from the channel to the central store
    _ <- forkIO $
        forever $ do
            (n, ev) <- readChan storeChan
            putStrLn $ "Sending back this message coming from microservice " ++ show n ++ " to the store: " ++ show ev
            WS.sendTextData conn $ JSON.encode $ HashMap.singleton ("messages" :: T.Text) ev

    -- Fork a thread that writes WS data to stdout.
    -- TODO remove
    forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg

handleMessage :: FilePath -> Connection -> NumClient -> Chan (NumClient, Message) -> Message -> IO ()
handleMessage f conn nc chan ev =
    do
        -- store the message in the message store
        unless
            (isType "InitiatedConnection" ev)
            (ES.appendMessage f ev)
        -- if the message is a InitiatedConnection, get the uuid list from it,
        -- and send back all the missing messages (with an added ack)
        when
            (isType "InitiatedConnection" ev)
            ( do
                let uuids = getUuids ev
                esevs <- ES.readMessages f
                let evs =
                        filter
                            ( \e -> case getMetaString "uuid" e of
                                Just u -> T.unpack u `notElem` uuids
                                Nothing -> False
                            )
                            esevs
                sendTextData conn $ JSON.encode $ HashMap.singleton ("messages" :: T.Text) evs
                putStrLn $ "Sent all missing " ++ show (length evs) ++ " messsages to client " ++ show nc
            )
        -- Send back and store an ACK to let the client know the message has been stored
        -- Except for messages that should be handled by another service
        let ev' = setProcessed ev
        unless (isType "InitiatedConnection" ev || isType "IdentifierAdded" ev) $ ES.appendMessage f ev'
        sendTextData conn $ JSON.encode $ HashMap.singleton ("messages" :: T.Text) [ev']
        -- send the msg to other connected clients
        putStrLn $ "Writing to the chan as client " ++ show nc
        writeChan chan (nc, ev)
        writeChan chan (nc, ev')

httpApp :: Options -> Application
httpApp (Options d _ _) request respond = do
    rawPathInfo request
        & decodeUtf8
        & T.append "Request "
        & T.unpack
        & putStrLn
    respond $ case pathInfo request of
        "static" : pathtail -> case pathtail of
            filename : _ ->
                let ct = BS.append "text/" (encodeUtf8 (contentType filename))
                 in responseFile status200 [("Content-Type", ct)] (d ++ "/static/" ++ T.unpack filename) Nothing
            _ -> responseLBS status200 [("Content-Type", "text/html")] ""
        "changelog" : _ -> responseFile status200 [("Content-Type", "text/html")] (d ++ "/static/changelog.html") Nothing
        _ -> responseFile status200 [("Content-Type", "text/html")] (d ++ "/index.html" :: String) Nothing

serve :: Options -> IO ()
serve (Options d p f) = do
    putStrLn $ "Modelyz Studio, serving on http://localhost:" ++ show p ++ "/"
    st <- newMVar 0
    chan <- newChan
    storeChan <- dupChan chan -- channel to the central message store
    putStrLn "Connecting to ws://localhost:8081/"
    _ <- forkIO $ runClient "localhost" 8081 "/" (clientApp storeChan)
    run 8080 $ websocketsOr defaultConnectionOptions (wsApp f chan st) $ httpApp (Options d p f)

main :: IO ()
main =
    serve =<< execParser opts
  where
    opts =
        info
            (options <**> helper)
            ( fullDesc
                <> progDesc "Studio helps you define your application domain"
                <> header "Modelyz Studio"
            )
