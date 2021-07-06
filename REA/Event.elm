module REA.Event exposing (Event, decode, encode, new)

import Json.Decode
import Json.Encode
import Prng.Uuid
import REA.EventType as ET exposing (EventType)


type alias Event =
    { name : String
    , uuid : Prng.Uuid.Uuid
    , etype : EventType

    --    , qty: Float
    --    , rtype: ResourceType
    --    , provider: Agent
    --    , receiver: Agent
    }


new : Prng.Uuid.Uuid -> Event
new uuid =
    { name = "Pizza order"
    , uuid = uuid
    , etype = ET.new uuid -- FIXME select the type
    }


encode : Event -> Json.Encode.Value
encode e =
    Json.Encode.object
        [ ( "name", Json.Encode.string e.name )
        , ( "uuid", Prng.Uuid.encode e.uuid )
        , ( "etype", ET.encode e.etype )
        ]


decode : Json.Decode.Decoder Event
decode =
    Json.Decode.map3 Event
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "uuid" Prng.Uuid.decoder)
        (Json.Decode.field "etype" ET.decode)
