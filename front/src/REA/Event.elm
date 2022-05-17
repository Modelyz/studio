module REA.Event exposing (Event, compare, decoder, encode, new)

import Json.Decode as Decode
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.EventType as ET exposing (EventType)
import Time exposing (millisToPosix, posixToMillis)


type alias Event =
    { name : String
    , uuid : Uuid
    , when : Time.Posix
    , type_ : EventType

    --    , qty: Float
    --    , rtype: ResourceType
    --    , provider: Agent
    --    , receiver: Agent
    }


new : String -> Uuid -> Time.Posix -> EventType -> Event
new name uuid when type_ =
    { name = name
    , uuid = uuid
    , when = when
    , type_ = type_
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
        (Decode.field "name" Decode.string)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "when" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))
        (Decode.field "type" ET.decoder)


compare : Event -> Int
compare =
    .when >> posixToMillis
