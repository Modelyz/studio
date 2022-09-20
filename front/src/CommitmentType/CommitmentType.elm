module CommitmentType.CommitmentType exposing (CommitmentType, compare, decoder, encode)

import Dict exposing (Dict)
import Group.Group exposing (Group)
import Ident.Identifier exposing (Identifier)
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type alias CommitmentType =
    { what : Type
    , uuid : Uuid
    , parent : Maybe Uuid
    , identifiers : Dict String Identifier
    , groups : Dict String Group
    , display : Dict String String
    }


encode : CommitmentType -> Encode.Value
encode ct =
    Encode.object <|
        [ ( "what", Type.encode ct.what )
        , ( "uuid", Uuid.encode ct.uuid )
        , ( "parent", Maybe.map Uuid.encode ct.parent |> Maybe.withDefault Encode.null )
        ]


decoder : Decode.Decoder CommitmentType
decoder =
    Decode.map6 CommitmentType
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)


compare : CommitmentType -> String
compare =
    toString


toString : CommitmentType -> String
toString =
    .uuid >> Uuid.toString
