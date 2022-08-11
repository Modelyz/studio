module CommitmentType.CommitmentType exposing (CommitmentType, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type alias CommitmentType =
    { what : Type
    , uuid : Uuid
    , parent : Maybe Uuid
    , group : Maybe Uuid
    }


encode : CommitmentType -> Encode.Value
encode ct =
    Encode.object <|
        [ ( "what", Type.encode ct.what )
        , ( "uuid", Uuid.encode ct.uuid )
        , ( "parent", Maybe.map Uuid.encode ct.parent |> Maybe.withDefault Encode.null )
        ]
            ++ (Maybe.map (\g -> [ ( "group", Uuid.encode g ) ]) ct.group |> Maybe.withDefault [])


decoder : Decode.Decoder CommitmentType
decoder =
    Decode.map4 CommitmentType
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)
        (Decode.maybe (Decode.field "group" Uuid.decoder))


compare : CommitmentType -> String
compare =
    toString


toString : CommitmentType -> String
toString =
    .uuid >> Uuid.toString
