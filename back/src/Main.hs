{-# LANGUAGE OverloadedStrings #-}

import Connection (Connection (..))
import Control.Concurrent (Chan, MVar, dupChan, forkIO, newChan, newMVar, putMVar, readChan, readMVar, takeMVar, threadDelay, writeChan)
import Control.Exception (SomeException (SomeException), catch)
import Control.Monad qualified as Monad (forever, unless, when)
import Control.Monad.Fix (fix)
import Data.Aeson qualified as JSON (eitherDecode, encode)
import Data.ByteString qualified as BS (append)
import Data.Function ((&))
import Data.Set as Set (Set, empty, insert)
import Data.Text qualified as T (Text, append, split, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock.POSIX
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID (nextRandom)
import Message (Effect (SendMessage), Message (..), Metadata (..), Payload (InitiatedConnection), appendMessage, getFlow, isType, metadata, payload, readMessages, setFlow, uuid)
import MessageFlow (MessageFlow (..))
import Network.HTTP.Types (status200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import Options.Applicative

-- dir, port, file
data Options = Options !String !Host !Port !FilePath !Host !Port

type NumClient = Int

type Host = String

type Port = Int

data State = State
    { messagesToSend :: [Message]
    , messagesToProcess :: [Message]
    , uuids :: Set UUID
    }
    deriving (Show)

type StateMV = MVar State

emptyState :: State
emptyState =
    State
        { messagesToSend = []
        , messagesToProcess = []
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

websocketServerApp :: FilePath -> Chan (NumClient, Message) -> MVar NumClient -> StateMV -> WS.ServerApp
websocketServerApp msgPath chan ncMV stateMV pending_conn = do
    elmChan <- dupChan chan -- channel to the browser application, dedicated to a connection
    -- accept a new connexion
    conn <- WS.acceptRequest pending_conn
    -- increment the client number
    nc <- takeMVar ncMV
    putMVar ncMV $! nc + 1
    putStrLn $ "\nBrowser " ++ show nc ++ " connected"

    _ <-
        -- wait for new message coming from other connected browsers through the chan
        -- and send them to the currently connected browser
        -- TODO: only Processed message should be forwarded to other browsers??
        forkIO $
            fix
                ( \loop -> do
                    (n, msg) <- readChan elmChan
                    Monad.when (n /= nc && not (msg `isType` "InitiatedConnection") && (getFlow msg == Requested || getFlow msg == Processed)) $ do
                        putStrLn $ "\nThread " ++ show nc ++ " got stuff through the chan from browser " ++ show n ++ ": Sent through WS : " ++ show msg
                        (WS.sendTextData conn . JSON.encode) msg
                    loop
                )

    -- handle message coming through websocket from the currently connected browser
    WS.withPingThread conn 30 (return ()) $
        Monad.forever $ do
            putStrLn $ "\nWaiting for new message from browser " ++ show nc
            message <- WS.receiveDataMessage conn
            putStrLn $ "\nReceived stuff through websocket from browser " ++ show nc ++ ". Handling it : " ++ show message
            case JSON.eitherDecode
                ( case message of
                    WS.Text bs _ -> WS.fromLazyByteString bs
                    WS.Binary bs -> WS.fromLazyByteString bs
                ) of
                Right msg -> do
                    case payload msg of
                        InitiatedConnection connection ->
                            -- if the message is a InitiatedConnection, get the uuid list from it,
                            -- and send back all the missing messages (with an added ack)
                            do
                                let remoteUuids = Connection.uuids connection
                                esevs <- readMessages msgPath
                                let msgs = filter (\e -> uuid (metadata e) `notElem` remoteUuids) esevs
                                mapM_ (WS.sendTextData conn . JSON.encode) msgs
                                putStrLn $ "\nSent all missing " ++ show (length msgs) ++ " messages to client " ++ show nc ++ ": " ++ show msgs
                        _ -> do
                            Monad.when (uuid (metadata msg) `notElem` Main.uuids st) $ do
                                -- store the message in the message store
                                appendMessage msgPath msg
                                putStrLn $ "\nStored message: " ++ show msg
                                -- update the state and get the list of new generated messages
                                state <- takeMVar stateMV
                                let (newState, effects) = update state msg
                                putMVar stateMV $! newState
                                putStrLn "updated state"
                -- TODO manage effects
                -- handle the message
                -- XXX remove handleMessageFromBrowser msgPath conn nc elmChan stateMV msg
                Left err -> putStrLn $ "\nError decoding incoming message: " ++ err

clientApp :: FilePath -> Chan (NumClient, Message) -> StateMV -> WS.ClientApp ()
clientApp msgPath storeChan stateMV conn = do
    putStrLn "Connected!"
    -- Just reconnected, first send an InitiatedConnection to the store
    newUuid <- UUID.nextRandom
    currentTime <- getPOSIXTime
    state <- takeMVar stateMV
    let initiatedConnection =
            Message
                (Metadata{uuid = newUuid, Message.when = currentTime, which = "studio", flow = Requested})
                (InitiatedConnection (Connection{lastMessageTime = 0, Connection.uuids = Main.uuids state}))
    _ <- WS.sendTextData conn $ JSON.encode initiatedConnection
    -- Just reconnected, send the messagesToSend to the Store
    catch
        ( Monad.unless (null (messagesToSend state)) $ do
            let tosend = messagesToSend state
            mapM_ (WS.sendTextData conn . JSON.encode) tosend
            putMVar stateMV $! state{messagesToSend = []}
        )
        ( \(SomeException _) -> do
            -- if something got wrong, put back the messages in the messagesToSend list
            st <- takeMVar stateMV
            putMVar stateMV $! state{messagesToSend = messagesToSend state <> messagesToSend st}
        )
    -- fork a thread to send back data from the channel to the central store
    _ <- forkIO $ do
        putStrLn "Waiting for messages coming from the client browsers"
        Monad.forever $ do
            (n, msg) <- readChan storeChan -- here we get all messages from all browsers
            Monad.unless (n == -1 || msg `isType` "InitiatedConnection") $ do
                putStrLn $ "\nForwarding to the store this message coming from browser " ++ show n ++ ": " ++ show msg
                st <- takeMVar stateMV
                let (newst, effects) = update st msg
                -- TODO: run the effects

                putMVar stateMV $! newst
                -- send to the Store
                WS.sendTextData conn $ JSON.encode msg

    Monad.forever $ do
        putStrLn "Waiting for messages coming from the store"
        messages <- WS.receiveDataMessage conn
        putStrLn $ "\nReceived stuff through websocket from the Store: " ++ show messages
        case JSON.eitherDecode
            ( case messages of
                WS.Text bs _ -> WS.fromLazyByteString bs
                WS.Binary bs -> WS.fromLazyByteString bs
            ) of
            Right msgs -> handleMessageFromStore stateMV msgPath (-1) storeChan msgs -- -1 is the Store. TODO use textual labels instead
            Left err -> putStrLn $ "\nError decoding incoming message: " ++ err

update :: (State, [Effect a]) -> Message -> (State, [Effect a])
update (state, es) msg =
    case flow (metadata msg) of
        Requested -> case payload msg of
            InitiatedConnection _ -> (state, [])
            _ ->
                -- set the message toSend
                ( state
                    { messagesToSend = messagesToSend state ++ [msg]
                    , Main.uuids = Set.insert (uuid (metadata msg)) (Main.uuids state)
                    }
                , es ++ [SendMessage "store" msg]
                )
        Received ->
            ( state
                { -- remove the message from toSent, put in toProcess

                  messagesToSend = filter (\m -> uuid (metadata m) /= uuid (metadata msg)) $ messagesToSend state
                , messagesToProcess = messagesToProcess state ++ [msg]
                }
            , es
            )
        Processed ->
            ( state
                { -- remove from toProcess
                  messagesToProcess = filter (\m -> uuid (metadata m) /= uuid (metadata msg)) $ messagesToProcess state
                }
            , es
            )
        Error _ -> (state, [])

handleMessageFromBrowser :: FilePath -> WS.Connection -> NumClient -> Chan (NumClient, Message) -> StateMV -> Message -> IO ()
handleMessageFromBrowser msgPath conn nc chan stateMV msg = do
    -- otherwise, store the msg, then send and store the Received version
    st <- readMVar stateMV
    Monad.when (uuid (metadata msg) `notElem` Main.uuids st) $ do
        -- store the message in the message store
        appendMessage msgPath msg
        putStrLn $ "\nStored message: " ++ show msg
        -- Send back a Received flow
        let receivedMsg = setFlow Received msg
        appendMessage msgPath receivedMsg
        WS.sendTextData conn $ JSON.encode receivedMsg
        putStrLn $ "\nReturned a Received flow: " ++ show receivedMsg
        -- Add it or remove to the pending list (if relevant) and keep the uuid
        state <- takeMVar stateMV
        putMVar stateMV $! fst $ update state msg
        putStrLn "updated state"
    -- send msg to other connected clients
    -- TODO: remove? Only Processed msgs should be sent?
    putStrLn $ "\nWriting to the chan as client " ++ show nc
    writeChan chan (nc, msg)

handleMessageFromStore :: StateMV -> FilePath -> NumClient -> Chan (NumClient, Message) -> Message -> IO ()
handleMessageFromStore stateMV msgPath nc chan msg = do
    st <- readMVar stateMV
    Monad.when (uuid (metadata msg) `notElem` Main.uuids st) $ do
        -- store the message in the message store
        appendMessage msgPath msg
        putStrLn $ "\nStored message" ++ show msg
        -- Add it or remove to the pending list (if relevant) and keep the uuid
        state <- takeMVar stateMV
        putMVar stateMV $! fst $ update state msg
        putStrLn "updated state"
        -- send msg to other connected clients
        putStrLn $ "\nWriting to the chan as client " ++ show nc
        writeChan chan (nc, msg)

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

reconnectClient :: Int -> POSIXTime -> Host -> Port -> FilePath -> Chan (NumClient, Message) -> StateMV -> IO ()
reconnectClient waitTime previousTime host port msgPath storeChan stateMV = do
    putStrLn $ "Waiting " ++ show waitTime ++ " seconds"
    threadDelay $ waitTime * 1000000
    putStrLn $ "Connecting to Store at ws://" ++ host ++ ":" ++ show port ++ "..."
    catch
        (WS.runClient host port "/" (clientApp msgPath storeChan stateMV))
        ( \(SomeException _) -> do
            disconnectTime <- getPOSIXTime
            let newWaitTime = if fromEnum (disconnectTime - previousTime) >= (1000000000000 * (maxWait + 1)) then 1 else min maxWait $ waitTime + 1
            reconnectClient newWaitTime disconnectTime host port msgPath storeChan stateMV
        )

effectThread :: Chan (NumClient, Message) -> StateMV -> IO ()
effectThread effectChan stateMV = do
    putStrLn "Starting effect thread..."
    (n, msg) <- readChan effectChan
    state <- takeMVar stateMV
    let (newState, effects) = update state msg
    putMVar stateMV newState

serve :: Options -> IO ()
serve (Options d host port msgPath storeHost storePort) = do
    ncMV <- newMVar 0 -- connection number
    chan <- newChan -- main channel, that will be duplicated once for the browsers, and one for the store
    -- Reconstruct the state
    putStrLn "Reconstructing the State..."
    msgs <- readMessages msgPath
    stateMV <- newMVar emptyState -- shared application state
    state <- takeMVar stateMV
    -- TODO could be rewritten with a Writer monad?
    let (newState, _) = foldl update (state, []) msgs -- TODO foldr or strict foldl ?
    putMVar stateMV newState
    putStrLn $ "Computed State:" ++ show newState
    -- keep connection to the Store
    firstTime <- getPOSIXTime
    storeChan <- dupChan chan -- output channel to the central message store
    _ <- forkIO $ reconnectClient 1 firstTime storeHost storePort msgPath storeChan stateMV
    -- effect Thread with its own channel
    effectChan <- dupChan chan
    _ <- forkIO $ effectThread effectChan stateMV
    putStrLn $ "Modelyz Studio, serving on http://" ++ show host ++ ":" ++ show port ++ "/"
    -- listen for client browsers
    Warp.run port $ websocketsOr WS.defaultConnectionOptions (websocketServerApp msgPath chan ncMV stateMV) $ httpApp (Options d host port msgPath storeHost storePort)

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
