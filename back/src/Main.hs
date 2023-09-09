{-# LANGUAGE OverloadedStrings #-}

import Connection (Connection (..))
import Control.Concurrent (Chan, MVar, dupChan, forkIO, newChan, newMVar, putMVar, readChan, readMVar, takeMVar, threadDelay, writeChan)
import Control.Exception (Handler (..), SomeException (SomeException), catches)
import Control.Monad qualified as Monad (forever, unless, when)
import Control.Monad.Fix (fix)
import Data.Aeson qualified as JSON (eitherDecode, encode)
import Data.ByteString qualified as BS (append)
import Data.Function ((&))
import Data.Set as Set (Set, delete, empty, insert)
import Data.Text qualified as T (Text, append, split, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock.POSIX
import Data.UUID.V4 qualified as UUID (nextRandom)
import Message (Message (..), Payload (InitiatedConnection), appendMessage, getFlow, isType, metadata, payload, readMessages)
import MessageFlow (MessageFlow (..))
import Metadata (Metadata (..), Origin (..))
import Network.HTTP.Types (status200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ConnectionException (..))
import Network.WebSockets qualified as WS
import Options.Applicative

-- dir, port, file
data Options = Options !String !Host !Port !FilePath !Host !Port

type Host = String

type Port = Int

data State = State
    { pending :: Set Message
    , uuids :: Set Metadata
    }
    deriving (Show)

type StateMV = MVar State

emptyState :: State
emptyState =
    State
        { pending = Set.empty
        , Main.uuids = Set.empty
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

serverApp :: FilePath -> Chan Message -> StateMV -> WS.ServerApp
serverApp msgPath chan stateMV pending_conn = do
    clientMV <- newMVar None
    browserChan <- dupChan chan -- channel to the browser application, dedicated to a connection
    -- accept a new connexion
    conn <- WS.acceptRequest pending_conn
    -- increment the client number
    putStrLn "\nNew browser connected"

    _ <- do
        -- SERVER WORKER THREAD (one per client thread)
        -- wait for new message coming from other connected browsers through the chan
        -- and send them to the currently connected browser
        putStrLn "Starting client worker thread"
        forkIO $
            fix
                ( \loop -> do
                    msg <- readChan browserChan
                    client <- readMVar clientMV
                    putStrLn $ "Connected client: " ++ show client -- display the client name instead
                    Monad.when
                        ( not (msg `isType` "InitiatedConnection")
                            && (getFlow msg == Processed)
                            && from (metadata msg) /= Front
                            && from (metadata msg) /= Store
                        )
                        $ do
                            putStrLn $ "\nGot stuff through the chan from browser. Sent through WS : " ++ show msg
                            (WS.sendTextData conn . JSON.encode) msg
                    loop
                )

    -- SERVER MAIN THREAD
    -- handle message coming through websocket from the currently connected browser
    WS.withPingThread conn 30 (return ()) $
        Monad.forever $ do
            putStrLn "\nWaiting for new message from browser"
            message <- WS.receiveDataMessage conn
            putStrLn $ "\nReceived stuff through websocket from browser. Handling it : " ++ show message
            case JSON.eitherDecode
                ( case message of
                    WS.Text bs _ -> WS.fromLazyByteString bs
                    WS.Binary bs -> WS.fromLazyByteString bs
                ) of
                Right msg -> do
                    case payload msg of
                        InitiatedConnection connection -> do
                            -- if the message is a InitiatedConnection, get the uuid list from it,
                            -- and send back all the missing messages (with an added ack)
                            -- get the name of the connected client
                            let from = Metadata.from $ metadata msg
                            _ <- takeMVar clientMV
                            putMVar clientMV from
                            putStrLn $ "Connected client: " ++ show from -- right place to do auth?
                            let remoteUuids = Connection.uuids connection
                            esevs <- readMessages msgPath
                            let msgs = filter (\e -> metadata e `notElem` remoteUuids) esevs
                            mapM_ (WS.sendTextData conn . JSON.encode) msgs
                            putStrLn $ "\nSent all missing " ++ show (length msgs) ++ " messages to client: " ++ show msgs
                        _ -> do
                            case flow (metadata msg) of
                                Requested -> do
                                    st <- readMVar stateMV
                                    Monad.when (((from . metadata) msg == Front) && metadata msg `notElem` Main.uuids st) $ do
                                        -- store the message in the message store
                                        appendMessage msgPath msg
                                        -- update the state and get the list of new generated messages
                                        state <- takeMVar stateMV
                                        let newState = update state msg
                                        putMVar stateMV $! newState
                                        putStrLn "updated state"
                                        writeChan browserChan msg
                                _ -> return ()
                -- TODO manage effects
                -- handle the message
                Left err -> putStrLn $ "\nError decoding incoming message: " ++ err

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
                (Metadata{uuid = newUuid, Metadata.when = currentTime, from = Studio, flow = Requested})
                (InitiatedConnection (Connection{lastMessageTime = 0, Connection.uuids = Main.uuids state}))
    _ <- WS.sendTextData conn $ JSON.encode initiatedConnection
    -- Just reconnected, send the pending messages to the Store
    mapM_ (WS.sendTextData conn . JSON.encode) (pending state)
    -- fork a thread to send back data from the channel to the central store
    -- CLIENT WORKER THREAD
    _ <- forkIO $ do
        putStrLn "Waiting for messages coming from the client browsers"
        Monad.forever $ do
            msg <- readChan storeChan -- here we get all messages from all browsers
            Monad.unless (from (metadata msg) /= Front) $ do
                case flow (metadata msg) of
                    Requested -> do
                        putStrLn $ "\nForwarding to the store this msg coming from browser: " ++ show msg
                        st <- takeMVar stateMV
                        -- TODO: run the effects
                        putMVar stateMV $! update st msg
                        -- send to the Store
                        WS.sendTextData conn $ JSON.encode msg
                    _ -> return ()

    -- CLIENT MAIN THREAD
    -- loop on the handling of messages incoming through websocket
    Monad.forever $ do
        putStrLn "Waiting for messages coming from the store"
        message <- WS.receiveDataMessage conn
        putStrLn $ "\nReceived msg through websocket from the Store: " ++ show message
        case JSON.eitherDecode
            ( case message of
                WS.Text bs _ -> WS.fromLazyByteString bs
                WS.Binary bs -> WS.fromLazyByteString bs
            ) of
            Right msg -> do
                case flow (metadata msg) of
                    Processed -> do
                        st' <- readMVar stateMV
                        Monad.when (from (metadata msg) == Ident && metadata msg `notElem` Main.uuids st') $ do
                            -- TODO XXXXXXXXXXXXXXXXXXXXXX : le message n'est pas stocké car son uuid existe dans le store. Alors qu'il devrait l'être car c'est un Processed
                            appendMessage msgPath msg
                            -- send msg to other connected clients
                            putStrLn "\nWriting to the chan"
                            writeChan storeChan msg
                            -- Add it or remove to the pending list (if relevant) and keep the uuid
                            st'' <- takeMVar stateMV
                            putMVar stateMV $! update st'' msg
                            putStrLn "updated state"
                    _ -> return ()
            Left err -> putStrLn $ "\nError decoding incoming message: " ++ err

update :: State -> Message -> State
update state msg =
    case flow (metadata msg) of
        Requested -> case payload msg of
            InitiatedConnection _ -> state
            _ ->
                state
                    { pending = Set.insert msg $ pending state
                    , Main.uuids = Set.insert (metadata msg) (Main.uuids state)
                    }
        Processed -> state{pending = Set.delete msg $ pending state}
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
            (\(_ :: ConnectionException) -> reconnectClient 1 previousTime host port msgPath storeChan stateMV)
        , Handler
            ( \(SomeException _) -> do
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
    Warp.run port $ websocketsOr WS.defaultConnectionOptions (serverApp msgPath chan stateMV) $ httpApp (Options d host port msgPath storeHost storePort)

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
