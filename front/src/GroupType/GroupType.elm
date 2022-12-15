module GroupType.GroupType exposing (GroupType, compare, decoder, encode)

import Hierarchy.Type as HType
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Prng.Uuid as Uuid exposing (Uuid)
import Tree


type alias GroupType =
    { what : HType.Type
    , uuid : Uuid
    , parent : Maybe Uuid
    , unique : Bool
    , treeType : Tree.Type
    }


encode : GroupType -> Encode.Value
encode gt =
    Encode.object
        [ ( "what", HType.encode gt.what )
        , ( "uuid", Uuid.encode gt.uuid )
        , ( "parent", Maybe.map Uuid.encode gt.parent |> Maybe.withDefault Encode.null )
        , ( "unique", Encode.bool gt.unique )
        , ( "treetype", Tree.encode gt.treeType )
        ]


decoder : Decode.Decoder GroupType
decoder =
    Decode.map5 GroupType
        (Decode.field "what" HType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)
        (Decode.field "unique" Decode.bool)
        (Decode.field "treetype" Tree.decoder)


compare : GroupType -> String
compare =
    .uuid >> Uuid.toString
