{-# LANGUAGE OverloadedStrings #-}

module Event (RawEvent, getIntValue, ack, getStringValue, excludeType, isType, isAfter) where

import qualified Data.Text as T (Text, pack, unpack)
import Data.UUID (UUID)
import Text.JSON (JSValue (JSObject, JSString), Result (..), decode, encode, makeObj, showJSON, toJSString, valFromObj)

type RawEvent = T.Text

type Time = Int

type Uuid = String -- uuid as a string generated in Elm

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

ack :: UUID -> Time -> Uuid -> RawEvent
ack uuid time origin =
  T.pack $
    encode $
      makeObj
        [ ("type", JSString $ toJSString "AckReceived"),
          ("uuid", JSString $ toJSString $ show uuid),
          ("posixtime", showJSON time),
          ("origin", JSString $ toJSString $ origin)
        ]
