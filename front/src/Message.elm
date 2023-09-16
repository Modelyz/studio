port module Message exposing (Message(..), MessageId, base, compare, compareMessageId, decoder, encode, exceptIC, getTime, readMessages, renewSeed, storeMessages, storeMessagesToSend)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import MessageFlow exposing (MessageFlow)
import Metadata exposing (Metadata)
import Payload exposing (Payload(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Time exposing (posixToMillis)



-- read messages from IDB


port readMessages : Encode.Value -> Cmd msg



-- store messages to IDB then send to WS


port storeMessages : Encode.Value -> Cmd msg



-- only store to IDB


port storeMessagesToSend : Encode.Value -> Cmd msg


port renewSeed : () -> Cmd msg



-- application/user messages --


type alias MessageId =
    ( Uuid, MessageFlow )


compareMessageId : MessageId -> String
compareMessageId ( uuid, flow ) =
    Uuid.toString uuid ++ "/" ++ MessageFlow.toString flow


type Message
    = Message Metadata Payload


base : Message -> Metadata
base (Message b _) =
    b


compare : Message -> Int
compare =
    -- TODO what if 2 messages at the exact same time?
    -- => also use a session uuid
    -- TODO : use UUID instead? see updatePending
    getTime >> posixToMillis


getTime : Message -> Time.Posix
getTime =
    base >> .when


exceptIC : List Message -> List Message
exceptIC es =
    List.filter
        (\(Message _ payload) ->
            case payload of
                InitiatedConnection _ ->
                    False

                _ ->
                    True
        )
        es



-- JSON encoding / decoding


encode : Message -> Encode.Value
encode (Message m p) =
    Encode.object
        [ ( "load", Payload.encode p )
        , ( "meta", Metadata.encode m )
        ]


decoder : Decoder Message
decoder =
    Decode.map2 Message
        (Decode.field "meta" Metadata.decoder)
        (Decode.field "load" Payload.decoder)
