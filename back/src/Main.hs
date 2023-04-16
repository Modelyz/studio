{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (Chan, MVar, dupChan, forkIO, newChan, newMVar, putMVar, readChan, takeMVar, threadDelay, writeChan)
import Control.Exception (SomeException (SomeException), catch)
import Control.Monad (forever, unless, when)
import Control.Monad.Fix (fix)
import Data.Aeson qualified as JSON (decode, encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS (append)
import Data.Function ((&))
import Data.List qualified as List
import Data.Set as Set (Set, delete, empty, insert)
import Data.Text qualified as T (Text, append, split, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock.POSIX
import Message (Message, appendMessage, getFlow, getMessages, getMetaString, getUuids, isType, readMessages, setFlow)
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

newtype State = State {pending :: Set Message}
  deriving (Show)

type StateMV = MVar State

emptyState :: State
emptyState = State {pending = Set.empty}

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
        Just msgs -> mapM_ (handleMessageFromBrowser msgPath conn nc elmChan stateMV) $ getMessages msgs
        Nothing -> sequence_ [putStrLn "\nError decoding incoming message"]

clientApp :: FilePath -> Chan (NumClient, Message) -> StateMV -> WS.ClientApp ()
clientApp msgPath storeChan stateMV conn = do
  putStrLn "Connected!"
  -- Just reconnected, first send the pending messages to the Store
  state <- takeMVar stateMV
  putMVar stateMV $! state {pending = Set.empty}
  catch
    ( unless (null (pending state)) $ do
        -- Send pending messages and store a flow=Sent version
        let pendings = pending state
        WS.sendTextData conn $ JSON.encode $ KeyMap.singleton "messages" pendings
        mapM_ (appendMessage msgPath . setFlow "Sent") pendings
    )
    ( \(SomeException _) -> do
        -- if something got wrong, put back the messages in the pending list
        st <- takeMVar stateMV
        putMVar stateMV $! state {pending = pending state <> pending st}
    )
  -- TODO: Use the Flow to determine if it has been received by the store, in case the store was not alive.
  -- fork a thread to send back data from the channel to the central store
  _ <- forkIO $ do
    putStrLn "Waiting for messages coming from the client browsers"
    forever $ do
      (n, ev) <- readChan storeChan
      unless (n == -1 || isType "InitiatedConnection" ev) $ do
        putStrLn $ "\nForwarding to the store this message coming from browser " ++ show n ++ ": " ++ show ev
        -- Remove the current msg from the pending list
        st <- takeMVar stateMV
        putMVar stateMV $! st {pending = updatePending ev $ pending st}
        -- send to the Store
        WS.sendTextData conn $ JSON.encode $ KeyMap.singleton "messages" [ev]

  forever $ do
    putStrLn "Waiting for messages coming from the store"
    messages <- WS.receiveDataMessage conn
    putStrLn $ "\nReceived stuff through websocket from the Store: " ++ show messages
    case JSON.decode
      ( case messages of
          WS.Text bs _ -> WS.fromLazyByteString bs
          WS.Binary bs -> WS.fromLazyByteString bs
      ) of
      Just msgs -> mapM (handleMessageFromStore msgPath conn (-1) storeChan) msgs -- -1 is the Store. TODO use textual labels instead
      Nothing -> sequence [putStrLn "\nError decoding incoming message"]

update :: State -> Message -> State
update state msg =
  -- update the pending field
  state {pending = updatePending msg $ pending state}

updatePending :: Message -> Set Message -> Set Message
updatePending msg pendings =
  case getFlow msg of
    Just "Requested" -> Set.insert msg pendings
    Just "Sent" -> Set.insert msg pendings
    Just "Processed" -> Set.delete msg pendings
    _ -> pendings

handleMessageFromBrowser :: FilePath -> WS.Connection -> NumClient -> Chan (NumClient, Message) -> StateMV -> Message -> IO ()
handleMessageFromBrowser msgPath conn nc chan stateMV msg =
  do
    -- store the message in the message store
    unless (isType "InitiatedConnection" msg) $ do
      appendMessage msgPath msg
      WS.sendTextData conn $ JSON.encode $ KeyMap.singleton "messages" $ List.singleton (setFlow "Sent" msg)
      putStrLn $ "\nStored message and returned a Sent flow: " ++ show msg
      -- Add it to the pending list (if Requested or Sent)
      state <- takeMVar stateMV
      putMVar stateMV $! update state msg
    -- if the message is a InitiatedConnection, get the uuid list from it,
    -- and send back all the missing messages (with an added ack)
    when (isType "InitiatedConnection" msg) $ do
      let uuids = getUuids msg
      esevs <- readMessages msgPath
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
handleMessageFromStore msgPath conn nc chan msg =
  do
    -- store the message in the message store
    unless (isType "InitiatedConnection" msg) $ do
      appendMessage msgPath msg
      putStrLn $ "\nStored message" ++ show msg
    -- if the message is a InitiatedConnection, get the uuid list from it,
    -- and send back all the missing messages (with an added ack)
    when (isType "InitiatedConnection" msg) $ do
      let uuids = getUuids msg
      esevs <- readMessages msgPath
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

connectClient :: Int -> POSIXTime -> Host -> Port -> FilePath -> Chan (NumClient, Message) -> StateMV -> IO ()
connectClient waitTime previousTime host port msgPath storeChan stateMV = do
  putStrLn $ "Waiting " ++ show waitTime ++ " seconds"

  threadDelay $ waitTime * 1000000
  putStrLn $ "Connecting to Store at ws://" ++ host ++ ":" ++ show port ++ "..."
  catch
    (WS.runClient host port "/" (clientApp msgPath storeChan stateMV))
    ( \(SomeException _) -> do
        disconnectTime <- getPOSIXTime
        let newWaitTime = if fromEnum (disconnectTime - previousTime) >= (1000000000000 * (maxWait + 1)) then 1 else min maxWait $ waitTime + 1
        connectClient newWaitTime disconnectTime host port msgPath storeChan stateMV
    )

serve :: Options -> IO ()
serve (Options d host port msgPath storeHost storePort) = do
  ncMV <- newMVar 0
  chan <- newChan -- initial channel
  stateMV <- newMVar emptyState
  storeChan <- dupChan chan -- output channel to the central message store
  firstTime <- getPOSIXTime
  -- Reconstruct the state
  putStrLn "Reconstructing the State"
  msgs <- readMessages msgPath
  state <- takeMVar stateMV
  let newState = foldl update state msgs
  putMVar stateMV newState
  putStrLn "Old state:"
  print state
  putStrLn "New State:"
  print newState
  -- keep connection to the Store
  _ <- forkIO $ connectClient 1 firstTime storeHost port msgPath storeChan stateMV
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
