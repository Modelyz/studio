module Group.Group exposing (Group, compare, decoder, encode)

import Dict exposing (Dict)
import Ident.Identifier exposing (Identifier)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope as Scope exposing (Scope)
import Type exposing (Type)


type alias Group =
    { what : Type
    , uuid : Uuid
    , type_ : Uuid
    , scope : Scope
    , identifiers : Dict String Identifier
    , display : Dict String String
    }


encode : Group -> Encode.Value
encode g =
    Encode.object
        [ ( "what", Type.encode g.what )
        , ( "uuid", Uuid.encode g.uuid )
        , ( "type", Uuid.encode g.type_ )
        , ( "scope", Scope.encode g.scope )
        ]


decoder : Decoder Group
decoder =
    Decode.map6 Group
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.field "scope" Scope.decoder)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)


compare : Group -> String
compare =
    .uuid >> Uuid.toString
