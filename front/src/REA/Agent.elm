module REA.Agent exposing (Agent, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)


type alias Agent =
    { uuid : Uuid
    , type_ : String
    }


encode : Agent -> Encode.Value
encode a =
    Encode.object
        [ ( "uuid", Uuid.encode a.uuid )
        , ( "type", Encode.string a.type_ )
        ]


decoder : Decoder Agent
decoder =
    Decode.map2 Agent
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Decode.string)


compare : Agent -> String
compare =
    .uuid >> Uuid.toString
