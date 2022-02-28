{-# LANGUAGE OverloadedStrings #-}

module EventStore (appendEvent, readEvents) where

import Control.Exception (SomeException (SomeException), catch)
import qualified Data.Text as T (Text, intercalate, lines, pack, unpack)
import qualified Data.Text.IO as IO (appendFile, readFile)
import Event (Event)
import Text.JSON (Result (..), decode, encode)

eventStore :: FilePath
eventStore = "eventstore.txt"

appendEvent :: Event -> IO ()
appendEvent event =
  IO.appendFile eventStore $ T.pack $ encode event ++ "\n"

-- read the event store from the specified date
-- and send all messages to the websocket connection
--
readEvents :: IO [Event]
readEvents =
  do
    es <- catch (IO.readFile eventStore) handleMissing
    case decode $ T.unpack $ "[" `mappend` (T.intercalate "," $ T.lines es) `mappend` "]" of
      Ok evs -> return evs
      Error _ -> return []
  where
    handleMissing :: SomeException -> IO T.Text
    handleMissing (SomeException _) = do
      putStrLn "Could not read EventSource"
      return ""
