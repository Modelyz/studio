{-# LANGUAGE OverloadedStrings #-}

module EventStore (appendEvent, readEvents) where

import Control.Exception (SomeException (SomeException), catch)
import Data.Aeson as JSON (decode, encode)
import qualified Data.Text.Lazy as T (Text, append, intercalate, lines)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.IO as IO (appendFile, readFile)
import Event (Event)

appendEvent :: FilePath -> Event -> IO ()
appendEvent f event =
    IO.appendFile f $ decodeUtf8 (JSON.encode event) `T.append` "\n"

-- read the event store
readEvents :: FilePath -> IO [Event]
readEvents f =
    do
        es <- catch (IO.readFile f) handleMissing
        case JSON.decode $ encodeUtf8 $ "[" `T.append` T.intercalate "," (T.lines es) `T.append` "]" of
            Just evs -> return evs
            Nothing -> return []
  where
    handleMissing :: SomeException -> IO T.Text
    handleMissing (SomeException _) = do
        putStrLn "Could not read EventSource"
        return ""
