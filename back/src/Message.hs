{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Message (getUuids, Uuid, Message, getInt, setProcessed, getMetaString, excludeType, isType, isAfter) where

import qualified Data.Aeson as JSON (Value (..), toJSON)
import qualified Data.Aeson.KeyMap as KeyMap
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
        (JSON.Object o) -> case KeyMap.lookup "meta" o of
            Just m -> case getInt "posixtime" m of
                Just et -> et >= t
                Nothing -> False
            _ -> False
        _ -> False

getInt :: KeyMap.Key -> Message -> Maybe Int
getInt k e =
    case e of
        (JSON.Object o) -> case KeyMap.lookup k o of
            Just (JSON.Number n) -> Just n >>= toBoundedInteger
            _ -> Nothing
        _ -> Nothing

getMetaString :: KeyMap.Key -> Message -> Maybe T.Text
getMetaString k e =
    case e of
        (JSON.Object o) -> case KeyMap.lookup "meta" o of
            (Just (JSON.Object m)) -> case KeyMap.lookup k m of
                Just (JSON.String s) -> Just s
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing

getString :: KeyMap.Key -> Message -> Maybe T.Text
getString k e =
    case e of
        (JSON.Object o) -> case KeyMap.lookup k o of
            Just (JSON.String s) -> Just s
            _ -> Nothing
        _ -> Nothing

getUuids :: Message -> [Uuid]
getUuids e =
    case e of
        (JSON.Object o) -> case KeyMap.lookup "load" o of
            (Just (JSON.Object m)) ->
                case KeyMap.lookup "uuids" m of
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
        JSON.Object keymap ->
            JSON.toJSON $
                KeyMap.alterF
                    ( \case
                        Just (JSON.Object meta) ->
                            Just $
                                Just $
                                    JSON.toJSON $
                                        KeyMap.alterF
                                            ( \case
                                                Just _ -> Just $ Just $ JSON.String "Processed"
                                                Nothing -> Nothing
                                            )
                                            "flow"
                                            meta
                        Just other -> Just $ Just other
                        Nothing -> Nothing
                    )
                    "meta"
                    keymap
        _ -> e

--    case makeObj
--        [ ("uuid", JSString $ toJSString $ show uuid)
--        , ("type", JSString $ toJSString "AckReceived")
--        , ("posixtime", showJSON time)
--        , ("origin", JSString $ toJSString $ origin)
--        ] of
--        JSObject o -> o
