module REA.Event exposing (Event, compare, decoder, encode, new)

import Json.Decode as Decode
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.EventType as ET exposing (EventType)
import Time exposing (millisToPosix, posixToMillis)


type alias Event =
    { name : String
    , uuid : Uuid
    , posixtime : Time.Posix
    , etype : EventType

    --    , qty: Float
    --    , rtype: ResourceType
    --    , provider: Agent
    --    , receiver: Agent
    }


new : String -> Uuid -> Time.Posix -> EventType -> Event
new name uuid posixtime etype =
    { name = name
    , uuid = uuid
    , posixtime = posixtime
    , etype = etype
    }


encode : Event -> Encode.Value
encode e =
    Encode.object
        [ ( "name", Encode.string e.name )
        , ( "uuid", Uuid.encode e.uuid )
        , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
        , ( "etype", ET.encode e.etype )
        ]


decoder : Decode.Decoder Event
decoder =
    Decode.map4 Event
        (Decode.field "name" Decode.string)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "posixtime" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))
        (Decode.field "etype" ET.decoder)


compare : Event -> Int
compare e =
    posixToMillis e.posixtime
