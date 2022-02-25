{-# LANGUAGE OverloadedStrings #-}

module Event (Event, getIntegerValue, getStringVal, getStringValue, isConnInit, isAfter) where

import qualified Data.Text as T (Text, unpack)
import Text.JSON (JSValue (JSObject), Result (..), decode, valFromObj)

type Event = T.Text

getStringVal :: String -> JSValue -> Result String
getStringVal key (JSObject o) = valFromObj key o
getStringVal _ _ = Error "Error: JSON message is not an object"

getIntegerVal :: String -> JSValue -> Result Int
getIntegerVal key (JSObject o) = valFromObj key o
getIntegerVal _ _ = Error "Error: JSON message is not an object"

isConnInit :: Event -> Bool
isConnInit ev = getStringValue "type" ev == "ConnectionInitiated"

isAfter :: Int -> Event -> Bool
isAfter t ev =
  case getIntegerValue "posixtime" ev of
    Just et -> et >= t
    Nothing -> False

getIntegerValue :: String -> Event -> Maybe Int
getIntegerValue key ev =
  case decode (T.unpack ev) >>= getIntegerVal key of
    Ok n -> Just n
    Error _ -> Nothing

getStringValue :: String -> Event -> String
getStringValue key ev =
  case decode (T.unpack ev) >>= getStringVal key of
    Ok v -> v
    Error s -> "Error: " ++ s
