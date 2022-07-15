{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Message (getUuids, Uuid, Message, getInt, setProcessed, getMetaString, excludeType, isType, isAfter) where

import qualified Data.Aeson as JSON (Value (..), toJSON)
import qualified Data.HashMap.Lazy as HashMap
import Data.Scientific
import qualified Data.Text as T (Text, unpack)
import qualified Data.Vector as Vector

type Message = JSON.Value

type Time = Int

type Uuid = String -- uuid as a string generated in Elm

isType :: T.Text -> Message -> Bool
isType t ev = getString "what" ev == Just t

excludeType :: T.Text -> [Message] -> [Message]
excludeType t = filter (not . isType t)

isAfter :: Time -> Message -> Bool
isAfter t ev =
    case ev of
        (JSON.Object o) -> case HashMap.lookup "meta" o of
            Just m -> case getInt "posixtime" m of
                Just et -> et >= t
                Nothing -> False
            _ -> False
        _ -> False

getInt :: T.Text -> Message -> Maybe Int
getInt k e =
    case e of
        (JSON.Object o) -> case HashMap.lookup k o of
            Just (JSON.Number n) -> Just n >>= toBoundedInteger
            _ -> Nothing
        _ -> Nothing

getMetaString :: T.Text -> Message -> Maybe T.Text
getMetaString k e =
    case e of
        (JSON.Object o) -> case HashMap.lookup "meta" o of
            (Just (JSON.Object m)) -> case HashMap.lookup k m of
                Just (JSON.String s) -> Just s
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing

getString :: T.Text -> Message -> Maybe T.Text
getString k e =
    case e of
        (JSON.Object o) -> case HashMap.lookup k o of
            Just (JSON.String s) -> Just s
            _ -> Nothing
        _ -> Nothing

getUuids :: Message -> [Uuid]
getUuids e =
    case e of
        (JSON.Object o) -> case HashMap.lookup "load" o of
            (Just (JSON.Object m)) ->
                case HashMap.lookup "uuids" m of
                    Just (JSON.Array uuids) ->
                        Vector.toList
                            ( fmap
                                ( \case
                                    JSON.String uuid -> T.unpack uuid
                                    _ -> ""
                                )
                                uuids
                            )
                    _ -> []
            _ -> []
        _ -> []

setProcessed :: Message -> Message
setProcessed e =
    case e of
        JSON.Object o -> case HashMap.lookup "meta" o of
            Just (JSON.Object m) ->
                JSON.toJSON $ HashMap.update (\_ -> Just $ JSON.toJSON $ HashMap.update (\_ -> Just $ JSON.String "Processed") "flow" m) "meta" o
            _ -> e
        _ -> e

--    case makeObj
--        [ ("uuid", JSString $ toJSString $ show uuid)
--        , ("type", JSString $ toJSString "AckReceived")
--        , ("posixtime", showJSON time)
--        , ("origin", JSString $ toJSString $ origin)
--        ] of
--        JSObject o -> o
