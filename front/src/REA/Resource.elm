module REA.Resource exposing (Resource, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)


type alias Resource =
    { uuid : Uuid
    , type_ : String
    }


compare : Resource -> String
compare =
    .uuid >> Uuid.toString


encode : Resource -> Encode.Value
encode r =
    Encode.object
        [ ( "uuid", Uuid.encode r.uuid )
        , ( "type", Encode.string r.type_ )
        ]


decoder : Decoder Resource
decoder =
    Decode.map2 Resource
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Decode.string)
