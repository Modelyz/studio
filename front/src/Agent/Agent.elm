module Agent.Agent exposing (Agent, compare, decoder, encode)

import Dict exposing (Dict)
import Group.Group exposing (Group)
import Ident.Identifier exposing (Identifier)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)
import Typed.Type as TType
import Value.Value exposing (Value)


type alias Agent =
    { what : TType.Type
    , uuid : Uuid
    , type_ : Uuid
    , identifiers : Dict String Identifier
    , values : Dict String Value
    , groups : Dict String Group
    , display : Dict String String
    }


encode : Agent -> Encode.Value
encode a =
    Encode.object <|
        [ ( "what", TType.encode a.what )
        , ( "uuid", Uuid.encode a.uuid )
        , ( "type", Uuid.encode a.type_ )
        ]


decoder : Decoder Agent
decoder =
    Decode.map7 Agent
        (Decode.field "what" TType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)


compare : Agent -> String
compare =
    .uuid >> Uuid.toString
