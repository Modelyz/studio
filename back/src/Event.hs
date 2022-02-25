{-# LANGUAGE OverloadedStrings #-}

module Event (Event, getIntegerValue, getStringVal, getStringValue, skipUntil) where

import qualified Data.Text as T (Text, unpack)
import Text.JSON (JSValue (JSObject), Result (..), decode, valFromObj)

type Event = T.Text

getStringVal :: String -> JSValue -> Result String
getStringVal key (JSObject o) = valFromObj key o
getStringVal _ _ = Error "Error: JSON message is not an object"

getIntegerVal :: String -> JSValue -> Result Int
getIntegerVal key (JSObject o) = valFromObj key o
getIntegerVal _ _ = Error "Error: JSON message is not an object"

skipUntil :: Int -> [Event] -> [Event]
skipUntil t =
  filter (\m -> getIntegerValue "posixtime" m >= t && getStringValue "type" m /= "ConnectionInitiated")

getIntegerValue :: String -> Event -> Int
getIntegerValue key ev =
  case decode (T.unpack ev) >>= getIntegerVal key of
    Ok n -> n
    Error _ -> 0

getStringValue :: String -> Event -> String
getStringValue key ev =
  case decode (T.unpack ev) >>= getStringVal key of
    Ok v -> v
    Error s -> "Error: " ++ s
