{-# LANGUAGE OverloadedStrings #-}

module Event (Event, getInt, ack, getString, excludeType, isType, isAfter) where

import qualified Data.Text as T (Text, pack, unpack)
import Data.UUID (UUID)
import Text.JSON (JSObject, JSValue (JSObject, JSString), Result (..), decode, encode, makeObj, showJSON, toJSString, valFromObj)

type Event = JSObject JSValue

type Time = Int

type Uuid = String -- uuid as a string generated in Elm

isType :: String -> Event -> Bool
isType t ev = getString "type" ev == Ok t

excludeType :: String -> [Event] -> [Event]
excludeType t = filter (\ev -> not $ isType t ev)

isAfter :: Int -> Event -> Bool
isAfter t ev =
  case getInt "posixtime" ev of
    Ok et -> et >= t
    Error _ -> False

getInt :: String -> Event -> Result Int
getInt =
  valFromObj

getString :: String -> Event -> Result String
getString =
  valFromObj

ack :: UUID -> Time -> Uuid -> Event
ack uuid time origin =
  case makeObj
    [ ("type", JSString $ toJSString "AckReceived"),
      ("uuid", JSString $ toJSString $ show uuid),
      ("posixtime", showJSON time),
      ("origin", JSString $ toJSString $ origin)
    ] of
    JSObject o -> o
