module Group.Group exposing (Group, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Scope.Scope as Scope exposing (Scope)
import Typed.Type as TType


type alias Group =
    { what : TType.Type
    , uuid : Uuid
    , type_ : Uuid

    -- TODO what's the scope of a group?
    , scope : Scope
    }


encode : Group -> Encode.Value
encode g =
    Encode.object
        [ ( "what", TType.encode g.what )
        , ( "uuid", Uuid.encode g.uuid )
        , ( "type", Uuid.encode g.type_ )
        , ( "scope", Scope.encode g.scope )
        ]


decoder : Decoder Group
decoder =
    Decode.map4 Group
        (Decode.field "what" TType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.field "scope" Scope.decoder)
