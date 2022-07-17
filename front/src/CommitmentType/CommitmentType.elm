module CommitmentType.CommitmentType exposing (CommitmentType, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid as Uuid exposing (Uuid)


type alias CommitmentType =
    { uuid : Uuid
    , type_ : Maybe Uuid
    }


encode : CommitmentType -> Encode.Value
encode at =
    Encode.object
        [ ( "uuid", Uuid.encode at.uuid )
        , ( "type", Maybe.map Uuid.encode at.type_ |> Maybe.withDefault Encode.null )
        ]


decoder : Decode.Decoder CommitmentType
decoder =
    Decode.map2 CommitmentType
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" <| Decode.maybe Uuid.decoder)


compare : CommitmentType -> String
compare =
    toString


toString : CommitmentType -> String
toString =
    .uuid >> Uuid.toString
