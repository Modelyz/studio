{-# LANGUAGE OverloadedStrings #-}

module Event (getIntegerValue, getStringVal, getStringValue, skipUntil) where

import Text.JSON (JSValue (JSObject), Result (..), decode, valFromObj)

getStringVal :: String -> JSValue -> Result String
getStringVal key (JSObject obj) = valFromObj key obj
getStringVal _ _ = Error "Error: JSON message is not an object"

getIntegerVal :: String -> JSValue -> Result Integer
getIntegerVal key (JSObject obj) = valFromObj key obj
getIntegerVal _ _ = Error "Error: JSON message is not an object"

skipUntil :: Integer -> [String] -> [String]
skipUntil limit =
  filter (\m -> getIntegerValue "posixtime" m >= limit && getStringValue "type" m /= "ConnectionInitiated")

getIntegerValue :: String -> String -> Integer
getIntegerValue key msg =
  case decode msg >>= getIntegerVal key of
    Ok n -> n
    Error _ -> 0

getStringValue :: String -> String -> String
getStringValue key msg =
  case decode msg >>= getStringVal key of
    Ok v -> v
    Error s -> "Error: " ++ s
