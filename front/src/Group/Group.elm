module Group.Group exposing (Group, compare, decoder, encode, fromUuid)

import Dict exposing (Dict)
import Dict exposing (Dict)
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
    , identifiers : Dict String String
    }


fromUuid : Dict String Group -> Uuid -> Maybe Group
fromUuid entities uuid =
    Dict.filter (\_ g -> g.uuid == uuid) entities |> Dict.values |> List.head


encode : Group -> Encode.Value
encode g =
    Encode.object
        [ ( "what", Type.encode g.what )
        , ( "uuid", Uuid.encode g.uuid )
        , ( "type", Uuid.encode g.type_ )
        , ( "def", Scope.encode g.scope )
        ]


decoder : Decoder Group
decoder =
    Decode.map5 Group
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.field "scope" Scope.decoder)
        (Decode.succeed Dict.empty)


compare : Group -> String
compare =
    .uuid >> Uuid.toString
