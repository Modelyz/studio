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
    threadDelay,
    writeChan,
 )
import Control.Exception (SomeException (SomeException), catch)
import Control.Monad (forever, unless, when)
import Control.Monad.Fix (fix)
import qualified Data.Aeson as JSON (decode, encode)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS (append)
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Text as T (Text, append, split, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock.POSIX
import Message (Message, appendMessage, getMessages, getMetaString, getUuids, isType, readMessages, setFlow)
import Network.HTTP.Types (status200)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Options.Applicative

-- dir, port, file
data Options = Options !String !Port !FilePath !Host !Port

type NumClient = Int

type Host = String
type Port = Int

newtype State = State {pending :: [Message]}
    deriving (Show)

type StateMV = MVar State

emptyState :: State
emptyState = State{pending = []}

options :: Parser Options
options =
    Options
        <$> strOption (short 'd' <> long "dir" <> value "." <> help "Directory containing the index file and static directory")
        <*> option auto (long "port" <> short 'p' <> metavar "PORT" <> value 8080 <> help "Bind socket to this port.  [default: 8080]")
        <*> strOption (short 'f' <> long "file" <> value "messagestore.txt" <> help "Filename of the file containing messages")
        <*> strOption (long "store_host" <> value "localhost" <> help "Hostname of the Store service. [default: localhost]")
        <*> option auto (long "store_port" <> metavar "STORE_PORT" <> value 8081 <> help "Port of the Store service.  [default: 8081]")

contentType :: T.Text -> T.Text
contentType filename = case reverse $ T.split (== '.') filename of
    "css" : _ -> "css"
    "js" : _ -> "javascript"
    _ -> "raw"

wsApp :: FilePath -> Chan (NumClient, Message) -> MVar NumClient -> WS.ServerApp
wsApp f chan ncMV pending_conn = do
    elmChan <- dupChan chan -- channel to the browser application, dedicated to a connection
    -- accept a new connexion
    conn <- WS.acceptRequest pending_conn
    -- increment the client number
    nc <- takeMVar ncMV
    putMVar ncMV (nc + 1)
    putStrLn $ "\nBrowser " ++ show nc ++ " connected"
    -- wait for new messages coming from other connected browsers through the chan
    -- and send them to the currently connected browser

    _ <-
        forkIO $
            fix
                ( \loop -> do
                    (n, ev) <- readChan elmChan
                    when (n /= nc && not (isType "InitiatedConnection" ev)) $ do
                        putStrLn $ "\nThread " ++ show nc ++ " got stuff through the chan from connected browser " ++ show n ++ ": Sent through WS : " ++ show ev
                        WS.sendTextData conn $ JSON.encode $ KeyMap.singleton "messages" [ev]
                    loop
                )

    -- handle messages coming through websocket from the currently connected browser
    WS.withPingThread conn 30 (return ()) $
        forever $ do
            putStrLn $ "\nWaiting for new messages from browser " ++ show nc
            messages <- WS.receiveDataMessage conn
            putStrLn $ "\nReceived stuff through websocket from Elm client " ++ show nc ++ ". Handling it : " ++ show messages
            case JSON.decode
                ( case messages of
                    WS.Text bs _ -> WS.fromLazyByteString bs
                    WS.Binary bs -> WS.fromLazyByteString bs
                ) of
                Just msgs -> mapM_ (handleMessageFromBrowser f conn nc elmChan) $ getMessages msgs
                Nothing -> sequence_ [putStrLn "\nError decoding incoming message"]

clientApp :: FilePath -> Chan (NumClient, Message) -> StateMV -> WS.ClientApp ()
clientApp f storeChan stateMV conn = do
    putStrLn "Connected!"
    -- TODO: Use the Flow to determine if it has been received by the store, in case the store was not alive.
    -- fork a thread to send back data from the channel to the central store
    _ <- forkIO $
        forever $ do
            (n, ev) <- readChan storeChan
            unless (n == -1 || isType "InitiatedConnection" ev) $ do
                putStrLn $ "\nSending back this message coming from browser " ++ show n ++ " to the Store: " ++ show ev
                WS.sendTextData conn $ JSON.encode $ KeyMap.singleton "messages" [ev]

    putStrLn "Starting message handler"
    forever $ do
        putStrLn "\nWaiting for new messages from the store"
        messages <- WS.receiveDataMessage conn
        putStrLn $ "\nReceived stuff through websocket from the Store: " ++ show messages
        case JSON.decode
            ( case messages of
                WS.Text bs _ -> WS.fromLazyByteString bs
                WS.Binary bs -> WS.fromLazyByteString bs
            ) of
            Just msgs -> mapM (handleMessageFromStore f conn (-1) storeChan) msgs -- -1 is the Store. TODO use textual labels instead
            Nothing -> sequence [putStrLn "\nError decoding incoming message"]

handleMessageFromBrowser :: FilePath -> WS.Connection -> NumClient -> Chan (NumClient, Message) -> Message -> IO ()
handleMessageFromBrowser f conn nc chan msg =
    do
        -- store the message in the message store
        unless (isType "InitiatedConnection" msg) $ do
            appendMessage f msg
            WS.sendTextData conn $ JSON.encode $ KeyMap.singleton "messages" $ List.singleton (setFlow "Sent" msg)
            putStrLn $ "\nStored message and returned a Sent flow: " ++ show msg
        -- if the message is a InitiatedConnection, get the uuid list from it,
        -- and send back all the missing messages (with an added ack)
        when (isType "InitiatedConnection" msg) $ do
            let uuids = getUuids msg
            esevs <- readMessages f
            let msgs =
                    filter
                        ( \e -> case getMetaString "uuid" e of
                            Just u -> T.unpack u `notElem` uuids
                            Nothing -> False
                        )
                        esevs
            WS.sendTextData conn $ JSON.encode $ KeyMap.singleton "messages" msgs
            putStrLn $ "\nSent all missing " ++ show (length msgs) ++ " messsages to client " ++ show nc ++ ": " ++ show (KeyMap.singleton "messages" msgs)
        -- send msg to other connected clients
        putStrLn $ "\nWriting to the chan as client " ++ show nc
        writeChan chan (nc, msg)

handleMessageFromStore :: FilePath -> WS.Connection -> NumClient -> Chan (NumClient, Message) -> Message -> IO ()
handleMessageFromStore f conn nc chan msg =
    do
        -- store the message in the message store
        unless (isType "InitiatedConnection" msg) $ do
            appendMessage f msg
            putStrLn $ "\nStored message" ++ show msg
        -- if the message is a InitiatedConnection, get the uuid list from it,
        -- and send back all the missing messages (with an added ack)
        when (isType "InitiatedConnection" msg) $ do
            let uuids = getUuids msg
            esevs <- readMessages f
            let msgs =
                    filter
                        ( \e -> case getMetaString "uuid" e of
                            Just u -> T.unpack u `notElem` uuids
                            Nothing -> False
                        )
                        esevs
            WS.sendTextData conn $ JSON.encode $ KeyMap.singleton "messages" msgs
            putStrLn $ "\nSent all missing " ++ show (length msgs) ++ " messsages to client " ++ show nc ++ ": " ++ show (KeyMap.singleton "messages" msgs)
        -- send msg to other connected clients
        putStrLn $ "\nWriting to the chan as client " ++ show nc
        writeChan chan (nc, msg)

httpApp :: Options -> Wai.Application
httpApp (Options d _ _ _ _) request respond = do
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

connectClient :: Int -> POSIXTime -> Host -> Port -> FilePath -> Chan (NumClient, Message) -> StateMV -> IO ()
connectClient waitTime time1 sh sp f storeChan stateMV = do
    putStrLn $ "time1 = " ++ show time1
    putStrLn $ "Waiting " ++ show waitTime ++ " seconds"
    threadDelay $ waitTime * 1000000
    putStrLn "Trying to connect..."
    catch
        (WS.runClient sh sp "/" (clientApp f storeChan stateMV))
        ( \(SomeException _) -> do
            time2 <- getPOSIXTime
            let newWaitTime = if fromEnum (time2 - time1) >= (1000000000000 * (maxWait + 1)) then 1 else min maxWait $ waitTime + 1
            connectClient newWaitTime time2 sh sp f storeChan stateMV
        )

serve :: Options -> IO ()
serve (Options d p f sh sp) = do
    ncMV <- newMVar 0
    chan <- newChan -- initial channel
    stateMV <- newMVar emptyState
    storeChan <- dupChan chan -- output channel to the central message store
    putStrLn $ "Connecting to Store at ws://" ++ sh ++ ":" ++ show sp ++ "/"
    firstTime <- getPOSIXTime
    _ <- forkIO $ connectClient 1 firstTime sh sp f storeChan stateMV

    putStrLn $ "Modelyz Studio, serving on http://localhost:" ++ show p ++ "/"
    Warp.run 8080 $ websocketsOr WS.defaultConnectionOptions (wsApp f chan ncMV) $ httpApp (Options d p f sh sp)

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
