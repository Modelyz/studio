module REA.Event exposing (Event, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.EventType as ET exposing (EventType)
import Time exposing (millisToPosix, posixToMillis)


type alias Event =
    { type_ : EventType
    , name : String
    , uuid : Uuid
    , when : Time.Posix

    --    , qty: Float
    --    , rtype: ResourceType
    --    , provider: Agent
    --    , receiver: Agent
    }


encode : Event -> Encode.Value
encode e =
    Encode.object
        [ ( "name", Encode.string e.name )
        , ( "uuid", Uuid.encode e.uuid )
        , ( "when", Encode.int <| posixToMillis e.when )
        , ( "type", ET.encode e.type_ )
        ]


decoder : Decode.Decoder Event
decoder =
    Decode.map4 Event
        (Decode.field "type" ET.decoder)
        (Decode.field "name" Decode.string)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "when" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))


compare : Event -> Int
compare =
    .when >> posixToMillis
