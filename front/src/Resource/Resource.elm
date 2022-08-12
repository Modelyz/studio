module Resource.Resource exposing (Resource, compare, decoder, encode)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type alias Resource =
    { what : Type
    , uuid : Uuid
    , type_ : Uuid
    , identifiers : Dict String String
    }


compare : Resource -> String
compare =
    .uuid >> Uuid.toString


encode : Resource -> Encode.Value
encode r =
    Encode.object <|
        [ ( "what", Type.encode r.what )
        , ( "uuid", Uuid.encode r.uuid )
        , ( "type", Uuid.encode r.type_ )
        ]


decoder : Decoder Resource
decoder =
    Decode.map4 Resource
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.succeed Dict.empty)
