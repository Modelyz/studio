module Agent.Agent exposing (Agent, compare, decoder, encode)

import Dict exposing (Dict)
import Item.Item as Item exposing (Item)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type alias Agent =
    { what : Type
    , uuid : Uuid
    , type_ : Uuid
    , identifiers : Dict String String
    }


encode : Agent -> Encode.Value
encode a =
    Encode.object <|
        [ ( "what", Type.encode a.what )
        , ( "uuid", Uuid.encode a.uuid )
        , ( "type", Uuid.encode a.type_ )
        ]


decoder : Decoder Agent
decoder =
    Decode.map4 Agent
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.succeed Dict.empty)


compare : Agent -> String
compare =
    .uuid >> Uuid.toString
