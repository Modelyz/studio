{-# LANGUAGE OverloadedStrings #-}

module EventStore (appendEvent, readEvents) where

import Control.Exception (SomeException (SomeException), catch)
import Data.Aeson as JSON (decode, encode)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import qualified Data.Text.Lazy as T (Text, append, fromStrict, intercalate, lines, pack, toStrict, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.IO as IO (appendFile, readFile)
import Event (Event, Uuid, getString)

eventStore :: FilePath
eventStore = "eventstore.txt"

appendEvent :: Event -> IO ()
appendEvent event =
    IO.appendFile eventStore $ (decodeUtf8 $ JSON.encode event) `T.append` "\n"

-- read the event store
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

getUuids :: IO (Set.Set Uuid)
getUuids = do
    evs <- readEvents
    return $ Set.fromList $ catMaybes $ map (\e -> fmap (T.unpack . T.fromStrict) $ getString "uuid" e) evs
