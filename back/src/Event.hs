{-# LANGUAGE OverloadedStrings #-}

module Event (RawEvent, getIntValue, getStringValue, excludeType, isType, isAfter) where

import qualified Data.Text as T (Text, unpack)
import Text.JSON (JSValue (JSObject), Result (..), decode, valFromObj)

type RawEvent = T.Text

type Date = Int

isType :: String -> RawEvent -> Bool
isType t ev = getStringValue "type" ev == Ok t

excludeType :: String -> [RawEvent] -> [RawEvent]
excludeType t = filter (\ev -> getStringValue "type" ev /= Ok t)

isAfter :: Int -> RawEvent -> Bool
isAfter t ev =
  case getIntValue "posixtime" ev of
    Ok et -> et >= t
    Error _ -> False

getIntValue :: String -> RawEvent -> Result Int
getIntValue key ev =
  decode (T.unpack ev) >>= (\(JSObject o) -> valFromObj key o)

getStringValue :: String -> RawEvent -> Result String
getStringValue key ev =
  decode (T.unpack ev) >>= (\(JSObject o) -> valFromObj key o)
