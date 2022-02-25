{-# LANGUAGE OverloadedStrings #-}

module Event (RawEvent, getIntegerValue, getStringVal, getStringValue, isConnInit, isAfter) where

import qualified Data.Text as T (Text, unpack)
import Text.JSON (JSValue (JSObject), Result (..), decode, valFromObj)

type RawEvent = T.Text

getStringVal :: String -> JSValue -> Result String
getStringVal key (JSObject o) = valFromObj key o
getStringVal _ _ = Error "Error: JSON message is not an object"

getIntegerVal :: String -> JSValue -> Result Int
getIntegerVal key (JSObject o) = valFromObj key o
getIntegerVal _ _ = Error "Error: JSON message is not an object"

isConnInit :: RawEvent -> Bool
isConnInit ev = getStringValue "type" ev == Just "ConnectionInitiated"

isAfter :: Int -> RawEvent -> Bool
isAfter t ev =
  case getIntegerValue "posixtime" ev of
    Just et -> et >= t
    Nothing -> False

getIntegerValue :: String -> RawEvent -> Maybe Int
getIntegerValue key ev =
  case decode (T.unpack ev) >>= getIntegerVal key of
    Ok n -> Just n
    Error _ -> Nothing

getStringValue :: String -> RawEvent -> Maybe String
getStringValue key ev =
  case decode (T.unpack ev) >>= getStringVal key of
    Ok v -> Just v
    Error s -> Nothing
