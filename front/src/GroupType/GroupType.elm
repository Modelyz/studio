module GroupType.GroupType exposing (GroupType, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode


type alias GroupType =
    { name : String
    }


encode : GroupType -> Value
encode g =
    Encode.object
        [ ( "name", Encode.string g.name )
        ]


decoder : Decoder GroupType
decoder =
    Decode.map GroupType
        (Decode.field "name" Decode.string)


compare : GroupType -> String
compare =
    .name
