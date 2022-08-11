module GroupType.GroupType exposing (GroupType, compare, decoder, encode, find)

import DictSet as Set exposing (DictSet)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type alias GroupType =
    { what : Type
    , uuid : Uuid
    , parent : Maybe Uuid
    }


find : Uuid -> DictSet String GroupType -> Maybe GroupType
find uuid gts =
    -- TODO consider moving that to the Metadata? Or replace GroupType with a type variable?
    Set.filter (\gt -> .uuid gt == uuid) gts
        |> Set.toList
        |> List.head


encode : GroupType -> Encode.Value
encode gt =
    Encode.object
        [ ( "what", Type.encode gt.what )
        , ( "uuid", Uuid.encode gt.uuid )
        , ( "parent", Maybe.map Uuid.encode gt.parent |> Maybe.withDefault Encode.null )
        ]


decoder : Decode.Decoder GroupType
decoder =
    Decode.map3 GroupType
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)


compare : GroupType -> String
compare =
    toString


toString : GroupType -> String
toString =
    .uuid >> Uuid.toString
