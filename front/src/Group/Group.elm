module Group.Group exposing (Group, compare, decoder, encode, fromUuid)

import DictSet as Set exposing (DictSet)
import Group.Definition as Definition exposing (Definition)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)


type alias Group =
    { uuid : Uuid
    , type_ : Uuid
    , def : Definition
    }


fromUuid : DictSet String Group -> Uuid -> Maybe Group
fromUuid entities uuid =
    Set.filter (\g -> g.uuid == uuid) entities |> Set.toList |> List.head


encode : Group -> Encode.Value
encode g =
    Encode.object
        [ ( "uuid", Uuid.encode g.uuid )
        , ( "type", Uuid.encode g.type_ )
        , ( "def", Definition.encode g.def )
        ]


decoder : Decoder Group
decoder =
    Decode.map3 Group
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.field "def" Definition.decoder)


compare : Group -> String
compare =
    .uuid >> Uuid.toString
