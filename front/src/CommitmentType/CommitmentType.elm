module CommitmentType.CommitmentType exposing (CommitmentType, compare, decoder, encode)

import Dict exposing (Dict)
import Group.Group exposing (Group)
import Hierarchy.Type as HType
import Ident.Identifier exposing (Identifier)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)
import Value.Value exposing (Value)


type alias CommitmentType =
    { what : HType.Type
    , uuid : Uuid
    , parent : Maybe Uuid
    }


encode : CommitmentType -> Encode.Value
encode ct =
    Encode.object <|
        [ ( "what", HType.encode ct.what )
        , ( "uuid", Uuid.encode ct.uuid )
        , ( "parent", Maybe.map Uuid.encode ct.parent |> Maybe.withDefault Encode.null )
        ]


decoder : Decode.Decoder CommitmentType
decoder =
    Decode.map3 CommitmentType
        (Decode.field "what" HType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)


compare : CommitmentType -> String
compare =
    toString


toString : CommitmentType -> String
toString =
    .uuid >> Uuid.toString
