module CommitmentType.CommitmentType exposing (CommitmentType, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid as Uuid exposing (Uuid)


type alias CommitmentType =
    { uuid : Uuid
    , type_ : Maybe Uuid
    , group : Maybe Uuid
    }


encode : CommitmentType -> Encode.Value
encode ct =
    Encode.object <|
        [ ( "uuid", Uuid.encode ct.uuid )
        , ( "type", Maybe.map Uuid.encode ct.type_ |> Maybe.withDefault Encode.null )
        ]
            ++ (Maybe.map (\g -> [ ( "group", Uuid.encode g ) ]) ct.group |> Maybe.withDefault [])


decoder : Decode.Decoder CommitmentType
decoder =
    Decode.map3 CommitmentType
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" <| Decode.maybe Uuid.decoder)
        (Decode.maybe (Decode.field "group" Uuid.decoder))


compare : CommitmentType -> String
compare =
    toString


toString : CommitmentType -> String
toString =
    .uuid >> Uuid.toString
