module Group.GroupType exposing (GroupType, compare, decoder, encode)

import Json.Decode exposing (Decoder, Value)
import Json.Encode
import REA.Entity exposing (Entity)


type alias GroupType =
    { name : String
    }


encode : GroupType -> Value
encode a =
    Json.Encode.object
        [ ( "name", Json.Encode.string a.name )
        ]


decoder : Decoder GroupType
decoder =
    Json.Decode.map GroupType
        (Json.Decode.field "name" Json.Decode.string)


compare : GroupType -> String
compare =
    .name
