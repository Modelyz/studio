module ProcessType.ProcessType exposing (ProcessType, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid as Uuid exposing (Uuid)


type alias ProcessType =
    { uuid : Uuid
    , type_ : Maybe Uuid
    }


encode : ProcessType -> Encode.Value
encode at =
    Encode.object
        [ ( "uuid", Uuid.encode at.uuid )
        , ( "type", Maybe.map Uuid.encode at.type_ |> Maybe.withDefault Encode.null )
        ]


decoder : Decode.Decoder ProcessType
decoder =
    Decode.map2 ProcessType
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" <| Decode.maybe Uuid.decoder)


compare : ProcessType -> String
compare =
    toString


toString : ProcessType -> String
toString =
    .uuid >> Uuid.toString
