{-# LANGUAGE OverloadedStrings #-}

import Connection (Connection (..))
import Control.Concurrent (Chan, MVar, dupChan, forkIO, newChan, newMVar, putMVar, readChan, readMVar, takeMVar, threadDelay, writeChan)
import Control.Exception (Handler (..), SomeException (SomeException), catches)
import Control.Monad qualified as Monad (forever, unless, when)
import Control.Monad.Fix (fix)
import Data.Aeson qualified as JSON (eitherDecode, encode)
import Data.ByteString qualified as BS (append)
import Data.Function ((&))
import Data.Map.Strict as Map (Map, delete, empty, insert)
import Data.Set as Set (Set, empty, insert)
import Data.Text qualified as T (Text, append, split, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock.POSIX
import Data.UUID.V4 qualified as UUID (nextRandom)
import Message (Message (..), addVisited, appendMessage, creator, getFlow, lastVisited, messageId, metadata, payload, readMessages)
import MessageFlow (MessageFlow (..))
import MessageId (MessageId)
import Metadata (Metadata (..))
import Network.HTTP.Types (status200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ConnectionException (..))
import Network.WebSockets qualified as WS
import Options.Applicative
import Payload (Payload (..))
import Service (Service (..))

-- dir, port, file
data Options = Options !String !Host !Port !FilePath !Host !Port

type Host = String

type Port = Int

data State = State
    { pending :: Map MessageId Message
    , uuids :: Set MessageId
    , syncing :: Bool
    }
    deriving (Show)

type StateMV = MVar State

myself :: Service
myself = Studio

emptyState :: State
emptyState =
    State
        { pending = Map.empty
        , Main.uuids = Set.empty
        , syncing = True
        }

options :: Parser Options
options =
    Options
        <$> strOption (short 'd' <> long "dir" <> value "." <> help "Directory containing the index file and static directory")
        <*> strOption (long "host" <> value "localhost" <> help "IP or Hostname to listen to. [default: localhost]")
        <*> option auto (long "port" <> short 'p' <> metavar "PORT" <> value 8080 <> help "Port number to listen to.  [default: 8080]")
        <*> strOption (short 'f' <> long "file" <> value "messagestore.txt" <> help "Filename of the file containing messages")
        <*> strOption (long "store_host" <> value "localhost" <> help "Hostname of the Store service. [default: localhost]")
        <*> option auto (long "store_port" <> metavar "STORE_PORT" <> value 8081 <> help "Port of the Store service.  [default: 8081]")

contentType :: T.Text -> T.Text
contentType filename = case reverse $ T.split (== '.') filename of
    "css" : _ -> "css"
    "js" : _ -> "javascript"
    _ -> "raw"

serverApp :: FilePath -> Chan Message -> WS.ServerApp
serverApp msgPath chan pending_conn = do
    clientMV <- newMVar None
    browserChan <- dupChan chan -- channel to the browser application, dedicated to a connection
    -- accept a new connexion
    conn <- WS.acceptRequest pending_conn
    -- increment the client number
    putStrLn "New browser connected"

    _ <- do
        -- SERVER WORKER THREAD (one per client thread)
        -- wait for new message coming from other connected browsers through the chan
        -- and send them to the currently connected browser
        putStrLn "Starting client worker thread"
        forkIO $
            fix
                ( \loop -> do
                    msg <- readChan browserChan
                    putStrLn $ "SERVER WORKER THREAD got this msg from the chan:\n" ++ show msg
                    client <- readMVar clientMV
                    putStrLn $ "Connected client: " ++ show client -- display the client name instead
                    case getFlow msg of
                        Requested -> case creator msg of
                            Front ->
                                Monad.when (lastVisited msg == Store) $
                                    (WS.sendTextData conn . JSON.encode) (addVisited myself msg)
                            _ -> return ()
                        Processed -> case creator msg of
                            Dumb -> do
                                putStrLn $ "Sending to " ++ show client
                                (WS.sendTextData conn . JSON.encode) (addVisited myself msg)
                            Ident -> do
                                putStrLn $ "Sending to " ++ show client
                                (WS.sendTextData conn . JSON.encode) (addVisited myself msg)
                            _ -> return ()
                        _ -> return ()
                    loop
                )

    -- SERVER MAIN THREAD
    -- handle message coming through websocket from the currently connected browser
    WS.withPingThread conn 30 (return ()) $
        Monad.forever $ do
            message <- WS.receiveDataMessage conn
            putStrLn $ "SERVER MAIN THREAD got this msg from browsers:\n" ++ show message
            case JSON.eitherDecode
                ( case message of
                    WS.Text bs _ -> WS.fromLazyByteString bs
                    WS.Binary bs -> WS.fromLazyByteString bs
                ) of
                Right msg -> do
                    let from = creator msg
                    case payload msg of
                        InitiatedConnection connection -> do
                            -- if the message is a InitiatedConnection, get the uuid list from it,
                            -- and send back all the missing messages
                            -- get the name of the connected client
                            _ <- takeMVar clientMV
                            putMVar clientMV from
                            putStrLn $ "Connected client: " ++ show from -- right place to do auth?
                            let remoteUuids = Connection.uuids connection
                            putStrLn $ "remoteUuids = " ++ show remoteUuids
                            esevs <- readMessages msgPath
                            putStrLn "Started syncing to browsers"
                            putStrLn "event store ="
                            print esevs
                            putStrLn "remote UUIDS = "
                            print remoteUuids
                            let msgs = filter (\m -> messageId m `notElem` remoteUuids) esevs
                            mapM_ (WS.sendTextData conn . JSON.encode . (if creator msg == myself then id else addVisited myself)) msgs
                            putStrLn $ "Sent all missing " ++ show (length msgs) ++ " messages to browsers:\n" ++ show msgs
                            -- send the InitiatedConnection terminaison to signal the sync is over
                            (WS.sendTextData conn . JSON.encode) $
                                Message
                                    (Metadata{uuid = uuid $ metadata msg, Metadata.when = when $ metadata msg, Metadata.from = [myself], Metadata.flow = Processed})
                                    (InitiatedConnection (Connection{lastMessageTime = 0, Connection.uuids = Set.empty}))
                        _ -> do
                            case flow (metadata msg) of
                                Requested -> case from of
                                    Front -> do
                                        writeChan browserChan msg
                                        appendMessage msgPath msg
                                    _ -> return ()
                                _ -> return ()
                Left err -> putStrLn $ "Error decoding incoming message:\n" ++ err

clientApp :: FilePath -> Chan Message -> StateMV -> WS.ClientApp ()
clientApp msgPath storeChan stateMV conn = do
    putStrLn "Connected!"
    -- Just reconnected, first send an InitiatedConnection to the store
    newUuid <- UUID.nextRandom
    currentTime <- getPOSIXTime
    state <- readMVar stateMV
    -- send an initiatedConnection
    let initiatedConnection =
            Message
                (Metadata{uuid = newUuid, Metadata.when = currentTime, from = [myself], flow = Requested})
                (InitiatedConnection (Connection{lastMessageTime = 0, Connection.uuids = Main.uuids state}))
    _ <- WS.sendTextData conn $ JSON.encode initiatedConnection
    -- Just reconnected, send the pending messages to the Store
    mapM_ (WS.sendTextData conn . JSON.encode . addVisited myself) (pending state)
    -- fork a thread to send back data from the channel to the central store
    -- CLIENT WORKER THREAD
    _ <- forkIO $ do
        Monad.forever $ do
            msg <- readChan storeChan -- here we get all messages from all browsers
            putStrLn $ "CLIENT WORKER THREAD got this msg from the chan:\n" ++ show msg
            case flow (metadata msg) of
                Requested -> case creator msg of
                    Front -> do
                        case lastVisited msg of
                            Front -> do
                                putStrLn "Forwarding to the store"
                                WS.sendTextData conn $ JSON.encode $ addVisited myself msg
                            _ -> return ()
                    _ -> return ()
                _ -> return ()

    -- CLIENT MAIN THREAD
    -- loop on the handling of messages incoming from Store
    Monad.forever $ do
        putStrLn "Waiting for messages coming from the store"
        message <- WS.receiveDataMessage conn
        putStrLn $ "CLIENT MAIN THREAD received msg through websocket from the Store:\n" ++ show message
        case JSON.eitherDecode
            ( case message of
                WS.Text bs _ -> WS.fromLazyByteString bs
                WS.Binary bs -> WS.fromLazyByteString bs
            ) of
            Right msg -> do
                st <- readMVar stateMV
                case flow (metadata msg) of
                    Requested -> case creator msg of
                        Front -> Monad.when (messageId msg `notElem` Main.uuids st) $ do
                            appendMessage msgPath msg
                            -- Add it or remove to the pending list (if relevant) and keep the uuid
                            st' <- takeMVar stateMV
                            putMVar stateMV $! update st' msg
                            putStrLn "updated state"
                            -- send msg to the worker thread and to other connected clients
                            Monad.unless (syncing st) $ do
                                putStrLn "Writing to the chan"
                                writeChan storeChan msg
                        _ -> return ()
                    Processed -> do
                        putStrLn $ "STATE = " ++ show st
                        case payload msg of
                            InitiatedConnection _ -> do
                                st'' <- takeMVar stateMV
                                putMVar stateMV $! st''{syncing = False}
                                putStrLn "Finished syncing from Store"
                            _ -> Monad.when (messageId msg `notElem` Main.uuids st) $ do
                                case creator msg of
                                    Dumb -> do
                                        appendMessage msgPath msg
                                        -- Add it or remove to the pending list (if relevant) and keep the uuid
                                        st'' <- takeMVar stateMV
                                        putMVar stateMV $! update st'' msg
                                        Monad.unless (syncing st'') $ do
                                            putStrLn "Writing to the chan"
                                            writeChan storeChan msg
                                        putStrLn "updated state"
                                    Ident -> do
                                        appendMessage msgPath msg
                                        -- Add it or remove to the pending list (if relevant) and keep the uuid
                                        st'' <- takeMVar stateMV
                                        putMVar stateMV $! update st'' msg
                                        Monad.unless (syncing st'') $ do
                                            putStrLn "Writing to the chan"
                                            writeChan storeChan msg
                                        putStrLn "updated state"
                                    _ -> return ()
                    _ -> return ()
            Left err -> putStrLn $ "Error decoding incoming message:\n" ++ err

update :: State -> Message -> State
update state msg =
    case flow (metadata msg) of
        Requested -> case payload msg of
            InitiatedConnection _ -> state
            _ ->
                state
                    { pending = Map.insert (messageId msg) msg $ pending state
                    , Main.uuids = Set.insert (messageId msg) (Main.uuids state)
                    }
        Processed ->
            state
                { pending = Map.delete (messageId msg) $ pending state
                , Main.uuids = Set.insert (messageId msg) (Main.uuids state)
                }
        Error _ -> state

httpApp :: Options -> Wai.Application
httpApp (Options d _ _ _ _ _) request respond = do
    Wai.rawPathInfo request
        & decodeUtf8
        & T.append "Request "
        & T.unpack
        & putStrLn
    respond $ case Wai.pathInfo request of
        "static" : pathtail -> case pathtail of
            filename : _ ->
                let ct = BS.append "text/" (encodeUtf8 (contentType filename))
                 in Wai.responseFile status200 [("Content-Type", ct)] (d ++ "/static/" ++ T.unpack filename) Nothing
            _ -> Wai.responseLBS status200 [("Content-Type", "text/html")] ""
        "changelog" : _ -> Wai.responseFile status200 [("Content-Type", "text/html")] (d ++ "/static/changelog.html") Nothing
        _ -> Wai.responseFile status200 [("Content-Type", "text/html")] (d ++ "/index.html" :: String) Nothing

maxWait :: Int
maxWait = 10

reconnectClient :: Int -> POSIXTime -> Host -> Port -> FilePath -> Chan Message -> StateMV -> IO ()
reconnectClient waitTime previousTime host port msgPath storeChan stateMV = do
    putStrLn $ "Waiting " ++ show waitTime ++ " seconds"
    threadDelay $ waitTime * 1000000
    putStrLn $ "Connecting to Store at ws://" ++ host ++ ":" ++ show port ++ "..."
    catches
        (WS.runClient host port "/" (clientApp msgPath storeChan stateMV))
        [ Handler
            ( \(e :: ConnectionException) -> do
                print e
                reconnectClient 1 previousTime host port msgPath storeChan stateMV
            )
        , Handler
            ( \(SomeException e) -> do
                print e
                disconnectTime <- getPOSIXTime
                let newWaitTime = if fromEnum (disconnectTime - previousTime) >= (1000000000000 * (maxWait + 1)) then 1 else min maxWait $ waitTime + 1
                reconnectClient newWaitTime disconnectTime host port msgPath storeChan stateMV
            )
        ]

serve :: Options -> IO ()
serve (Options d host port msgPath storeHost storePort) = do
    chan <- newChan -- main channel, that will be duplicated once for the browsers, and one for the store
    -- Reconstruct the state
    putStrLn "Reconstructing the State..."
    msgs <- readMessages msgPath
    stateMV <- newMVar emptyState -- shared application state
    state <- takeMVar stateMV
    let newState = foldl update state msgs -- TODO foldr or strict foldl ?
    putMVar stateMV newState
    putStrLn $ "Computed State:" ++ show newState
    -- keep connection to the Store
    firstTime <- getPOSIXTime
    storeChan <- dupChan chan -- output channel to the central message store
    _ <- forkIO $ reconnectClient 1 firstTime storeHost storePort msgPath storeChan stateMV
    putStrLn $ "Modelyz Studio, serving on http://" ++ show host ++ ":" ++ show port ++ "/"
    -- listen for client browsers
    Warp.run port $ websocketsOr WS.defaultConnectionOptions (serverApp msgPath chan) $ httpApp (Options d host port msgPath storeHost storePort)

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
