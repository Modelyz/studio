{-# LANGUAGE OverloadedStrings #-}

module Event (RawEvent, getIntValue, getStringValue, isConnInit, isAfter) where

import qualified Data.Text as T (Text, unpack)
import Text.JSON (JSValue (JSObject), Result (..), decode, valFromObj)

type RawEvent = T.Text

isConnInit :: RawEvent -> Bool
isConnInit ev = getStringValue "type" ev == Just "ConnectionInitiated"

isAfter :: Int -> RawEvent -> Bool
isAfter t ev =
  case getIntValue "posixtime" ev of
    Just et -> et >= t
    Nothing -> False

getIntValue :: String -> RawEvent -> Maybe Int
getIntValue key ev =
  case decode (T.unpack ev) >>= (\(JSObject o) -> valFromObj key o) of
    Ok n -> Just n
    Error _ -> Nothing

getStringValue :: String -> RawEvent -> Maybe String
getStringValue key ev =
  case decode (T.unpack ev) >>= (\(JSObject o) -> valFromObj key o) of
    Ok v -> Just v
    Error s -> Nothing
