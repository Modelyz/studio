module Resource.Resource exposing (Resource, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Typed.Type as TType


type alias Resource =
    { what : TType.Type
    , uuid : Uuid
    , type_ : Uuid
    }


encode : Resource -> Encode.Value
encode r =
    Encode.object <|
        [ ( "what", TType.encode r.what )
        , ( "uuid", Uuid.encode r.uuid )
        , ( "type_", Uuid.encode r.type_ )
        ]


decoder : Decoder Resource
decoder =
    Decode.map3 Resource
        (Decode.field "what" TType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type_" Uuid.decoder)
