{-# LANGUAGE OverloadedStrings #-}

module Event (getUuids, Uuid, Event, getInt, setProcessed, getString, excludeType, isType, isAfter) where

import qualified Data.Aeson as JSON (Value (..), toJSON)
import qualified Data.HashMap.Lazy as HashMap
import Data.Scientific
import qualified Data.Text as T (Text, unpack)
import qualified Data.Vector as Vector

type Event = JSON.Value

type Time = Int

type Uuid = String -- uuid as a string generated in Elm

isType :: T.Text -> Event -> Bool
isType t ev = getString "what" ev == Just t

excludeType :: T.Text -> [Event] -> [Event]
excludeType t = filter (\ev -> not $ isType t ev)

isAfter :: Time -> Event -> Bool
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

getUuids :: Event -> [Uuid]
getUuids e =
    case e of
        (JSON.Object o) -> case (HashMap.lookup "uuids" o) of
            Just (JSON.Array uuids) ->
                Vector.toList
                    ( fmap
                        ( \v -> case v of
                            JSON.String uuid -> T.unpack uuid
                            _ -> ""
                        )
                        uuids
                    )
            _ -> []
        _ -> []

setProcessed :: Event -> Event
setProcessed e =
    case e of
        JSON.Object o -> JSON.toJSON $ HashMap.update (\_ -> Just $ JSON.String "Processed") "flow" o
        _ -> e

--    case makeObj
--        [ ("uuid", JSString $ toJSString $ show uuid)
--        , ("type", JSString $ toJSString "AckReceived")
--        , ("posixtime", showJSON time)
--        , ("origin", JSString $ toJSString $ origin)
--        ] of
--        JSObject o -> o
