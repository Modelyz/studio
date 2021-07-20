module REA.Event exposing (Event, compare, decoder, encode, new)

import Json.Decode
import Json.Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.EventType as ET exposing (EventType)


type alias Event =
    { name : String
    , uuid : Uuid
    , etype : EventType

    --    , qty: Float
    --    , rtype: ResourceType
    --    , provider: Agent
    --    , receiver: Agent
    }


new : Uuid -> Event
new uuid =
    { name = "Pizza order"
    , uuid = uuid
    , etype = ET.new uuid -- FIXME select the type
    }


encode : Event -> Json.Encode.Value
encode e =
    Json.Encode.object
        [ ( "name", Json.Encode.string e.name )
        , ( "uuid", Uuid.encode e.uuid )
        , ( "etype", ET.encode e.etype )
        ]


decoder : Json.Decode.Decoder Event
decoder =
    Json.Decode.map3 Event
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "uuid" Uuid.decoder)
        (Json.Decode.field "etype" ET.decoder)


compare : Event -> String
compare e =
    Uuid.toString e.uuid
