module GroupType.GroupType exposing (GroupType, compare, decoder, encode, find)

import Dict exposing (Dict)
import Ident.Identifier exposing (Identifier)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type alias GroupType =
    { what : Type
    , uuid : Uuid
    , parent : Maybe Uuid
    , identifiers : Dict String String
    }


find : Uuid -> Dict String GroupType -> Maybe GroupType
find uuid gts =
    -- TODO consider moving that to the Metadata? Or replace GroupType with a type variable?
    Dict.filter (\_ gt -> .uuid gt == uuid) gts
        |> Dict.values
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
    Decode.map4 GroupType
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)
        (Decode.succeed Dict.empty)


compare : GroupType -> String
compare =
    toString


toString : GroupType -> String
toString =
    .uuid >> Uuid.toString
