module MessageId exposing (MessageId, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import MessageFlow exposing (MessageFlow)
import Prng.Uuid as Uuid exposing (Uuid)
import Util exposing (encodeTuple)


type alias MessageId =
    ( Uuid, MessageFlow )


encode : MessageId -> Encode.Value
encode mid =
    encodeTuple Uuid.encode MessageFlow.encode mid


decoder : Decode.Decoder MessageId
decoder =
    Decode.map2 Tuple.pair
        (Decode.index 0 Uuid.decoder)
        (Decode.index 1 MessageFlow.decoder)


compare : MessageId -> String
compare ( uuid, flow ) =
    Uuid.toString uuid ++ ":" ++ MessageFlow.toString flow
