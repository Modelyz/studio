{-# LANGUAGE OverloadedStrings #-}

module Event (Event, getIntegerValue, getStringVal, getStringValue, skipUntil) where

import Text.JSON (JSValue (JSObject), Result (..), decode, valFromObj)

type Event = String

getStringVal :: String -> JSValue -> Result String
getStringVal key (JSObject obj) = valFromObj key obj
getStringVal _ _ = Error "Error: JSON message is not an object"

getIntegerVal :: String -> JSValue -> Result Int
getIntegerVal key (JSObject obj) = valFromObj key obj
getIntegerVal _ _ = Error "Error: JSON message is not an object"

skipUntil :: Int -> [Event] -> [Event]
skipUntil limit =
  filter (\m -> getIntegerValue "posixtime" m >= limit && getStringValue "type" m /= "ConnectionInitiated")

getIntegerValue :: String -> Event -> Int
getIntegerValue key msg =
  case decode msg >>= getIntegerVal key of
    Ok n -> n
    Error _ -> 0

getStringValue :: String -> Event -> String
getStringValue key msg =
  case decode msg >>= getStringVal key of
    Ok v -> v
    Error s -> "Error: " ++ s
