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
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS (append)
import Data.Function ((&))
import Data.List ()
import qualified Data.Text as T (Text, append, split, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Event (Event, getMetaString, getUuids, isType, setProcessed)
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
import Options.Applicative

-- dir, port, file
data Options = Options String Port FilePath

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
        <*> strOption (short 'f' <> long "file" <> value "eventstore.txt" <> help "Filename of the file containing events")

contentType :: T.Text -> T.Text
contentType filename = case reverse $ T.split (== '.') filename of
    "css" : _ -> "css"
    "js" : _ -> "javascript"
    _ -> "raw"

wsApp :: FilePath -> Chan (NumClient, Event) -> WSState -> ServerApp
wsApp f chan st pending_conn = do
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
            fix
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
                case JSON.decode
                    ( case messages of
                        Text bs _ -> fromLazyByteString bs
                        Binary bs -> fromLazyByteString bs
                    ) of
                    Just evs -> mapM (handleEvent f conn nc chan') evs
                    Nothing -> sequence [putStrLn "Error decoding incoming message"]

handleEvent :: FilePath -> Connection -> NumClient -> Chan (NumClient, Event) -> Event -> IO ()
handleEvent f conn nc chan ev =
    do
        -- store the event in the event store
        unless
            (isType "ConnectionInitiated" ev)
            (ES.appendEvent f ev)
        -- if the event is a ConnectionInitiated, get the uuid list from it,
        -- and send back all the missing events (with an added ack)
        when
            (isType "ConnectionInitiated" ev)
            ( do
                let uuids = getUuids ev
                esevs <- ES.readEvents f
                let evs =
                        filter
                            ( \e -> case getMetaString "uuid" e of
                                Just u -> T.unpack u `notElem` uuids
                                Nothing -> False
                            )
                            esevs
                sendTextData conn $ JSON.encode evs
                putStrLn $ "Sent all missing " ++ show (length evs) ++ " messsages to client " ++ show nc
            )
        -- Send back and store an ACK to let the client know the message has been stored
        -- Except for events that should be handled by another service
        let ev' = setProcessed ev
        unless (isType "ConnectionInitiated" ev || isType "IdentifierAdded" ev) $ ES.appendEvent f ev'
        sendTextData conn $ JSON.encode [ev']
        -- send the msg to other connected clients
        putStrLn $ "Writing to the chan as client " ++ show nc
        writeChan chan (nc, ev)
        writeChan chan (nc, ev')

-- TODO send the event back to the central event store so that it be handled by other microservices.

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
        _ -> responseFile status200 [("Content-Type", "text/html")] (d ++ "/index.html" :: String) Nothing

serve :: Options -> IO ()
serve (Options d p f) = do
    print $ "http://localhost:" ++ show p ++ "/"
    st <- newMVar 0
    chan <- newChan
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
