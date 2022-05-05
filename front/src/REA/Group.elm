module REA.Group exposing (Group, compare, decoder, encode)

import Json.Decode exposing (Decoder, Value)
import Json.Encode
import Prng.Uuid
import REA.Entity exposing (Entity)


type alias Group =
    { name : String
    , entity : Entity
    }


encode : Group -> Value
encode a =
    Json.Encode.object
        [ ( "name", Json.Encode.string a.name )
        , ( "entity", REA.Entity.encode a.entity )
        ]


decoder : Decoder Group
decoder =
    Json.Decode.map2 Group
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "entity" REA.Entity.decoder)


compare : Group -> String
compare =
    .name
