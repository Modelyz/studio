{-# LANGUAGE OverloadedStrings #-}

module EventStore (appendEvent, readEvents) where

import Control.Exception (SomeException (SomeException), catch)
import Data.Aeson as JSON (decode, encode)
import qualified Data.Text.Lazy as T (Text, append, intercalate, lines, pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.IO as IO (appendFile, readFile)
import Event (Event)

eventStore :: FilePath
eventStore = "eventstore.txt"

appendEvent :: Event -> IO ()
appendEvent event =
    IO.appendFile eventStore $ (decodeUtf8 $ JSON.encode event) `T.append` "\n"

-- read the event store from the specified date
-- and send all messages to the websocket connection
--
readEvents :: IO [Event]
readEvents =
    do
        es <- catch (IO.readFile eventStore) handleMissing
        case JSON.decode $ encodeUtf8 $ "[" `T.append` (T.intercalate "," $ T.lines es) `T.append` "]" of
            Just evs -> return evs
            Nothing -> return []
  where
    handleMissing :: SomeException -> IO T.Text
    handleMissing (SomeException _) = do
        putStrLn "Could not read EventSource"
        return ""
