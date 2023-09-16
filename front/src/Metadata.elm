module Metadata exposing (Metadata, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import MessageFlow exposing (MessageFlow)
import Prng.Uuid as Uuid exposing (Uuid)
import Service exposing (Service)
import Time exposing (millisToPosix, posixToMillis)


type alias Metadata =
    { uuid : Uuid
    , when : Time.Posix -- when asked?
    , from : List Service -- the path of visited services, the last is the creator
    , flow : MessageFlow
    }


compare : Metadata -> String
compare m =
    MessageFlow.toString m.flow ++ ":" ++ Uuid.toString m.uuid ++ ":" ++ String.join "→" (List.map Service.toString m.from)


encode : Metadata -> Encode.Value
encode b =
    Encode.object
        [ ( "uuid", Uuid.encode b.uuid )
        , ( "when", Encode.int <| posixToMillis b.when )
        , ( "from", Encode.list Service.encode b.from )
        , ( "flow", MessageFlow.encode b.flow )
        ]


decoder : Decoder Metadata
decoder =
    Decode.map4 Metadata
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "when" Decode.int |> Decode.andThen (millisToPosix >> Decode.succeed))
        (Decode.field "from" <| Decode.list Service.decoder)
        (Decode.field "flow" MessageFlow.decoder)
