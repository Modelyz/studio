module REA.Group exposing (Group, compare, decoder, encode)

import Json.Decode exposing (Decoder, Value)
import Json.Encode
import REA.Entity exposing (Entity)


type alias Group =
    { name : String

    -- TODO how to define what is in the group?
    }


encode : Group -> Value
encode a =
    Json.Encode.object
        [ ( "name", Json.Encode.string a.name )
        ]


decoder : Decoder Group
decoder =
    Json.Decode.map Group
        (Json.Decode.field "name" Json.Decode.string)


compare : Group -> String
compare =
    .name
