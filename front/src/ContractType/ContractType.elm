module ContractType.ContractType exposing (ContractType, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid as Uuid exposing (Uuid)


type alias ContractType =
    { uuid : Uuid
    , type_ : Maybe Uuid
    }


encode : ContractType -> Encode.Value
encode at =
    Encode.object
        [ ( "uuid", Uuid.encode at.uuid )
        , ( "type", Maybe.map Uuid.encode at.type_ |> Maybe.withDefault Encode.null )
        ]


decoder : Decode.Decoder ContractType
decoder =
    Decode.map2 ContractType
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" <| Decode.maybe Uuid.decoder)


compare : ContractType -> String
compare =
    toString


toString : ContractType -> String
toString =
    .uuid >> Uuid.toString
