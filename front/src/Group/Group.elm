module Group.Group exposing (Group, compare, decoder, encode, fromUuid)

import DictSet as Set exposing (DictSet)
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
    }


fromUuid : DictSet String Group -> Uuid -> Maybe Group
fromUuid entities uuid =
    Set.filter (\g -> g.uuid == uuid) entities |> Set.toList |> List.head


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
    Decode.map4 Group
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.field "scope" Scope.decoder)


compare : Group -> String
compare =
    .uuid >> Uuid.toString
