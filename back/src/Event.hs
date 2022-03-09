{-# LANGUAGE OverloadedStrings #-}

module Event (Event, getInt, setProcessed, getString, excludeType, isType, isAfter) where

import qualified Data.Aeson as JSON (Value (..), toJSON)
import qualified Data.ByteString.Lazy as LBS (ByteString)
import qualified Data.HashMap.Lazy as HashMap
import Data.Scientific
import qualified Data.Text as T (Text, pack, unpack)
import Data.UUID (UUID)

type Event = JSON.Value

type Time = Int

type Uuid = String -- uuid as a string generated in Elm

isType :: T.Text -> Event -> Bool
isType t ev = getString "type" ev == Just t

excludeType :: T.Text -> [Event] -> [Event]
excludeType t = filter (\ev -> not $ isType t ev)

isAfter :: Int -> Event -> Bool
isAfter t ev =
    case getInt "posixtime" ev of
        Just et -> et >= t
        Nothing -> False

getInt :: T.Text -> Event -> Maybe Int
getInt k e =
    case e of
        (JSON.Object o) -> case (HashMap.lookup k o) of
            Just (JSON.Number n) -> Just n >>= toBoundedInteger
            _ -> Nothing
        _ -> Nothing

getString :: T.Text -> Event -> Maybe T.Text
getString k e =
    case e of
        (JSON.Object o) -> case HashMap.lookup k o of
            Just (JSON.String s) -> Just s
            _ -> Nothing
        _ -> Nothing

setProcessed :: Event -> Event
setProcessed e =
    case e of
        JSON.Object o -> JSON.toJSON $ HashMap.update (\v -> Just $ JSON.String "Processed") "flow" o
        _ -> e

--    case makeObj
--        [ ("uuid", JSString $ toJSString $ show uuid)
--        , ("type", JSString $ toJSString "AckReceived")
--        , ("posixtime", showJSON time)
--        , ("origin", JSString $ toJSString $ origin)
--        ] of
--        JSObject o -> o
