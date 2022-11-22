module ContractType.ContractType exposing (ContractType, decoder, encode)

import Hierarchy.Type as HType
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Prng.Uuid as Uuid exposing (Uuid)


type alias ContractType =
    { what : HType.Type
    , uuid : Uuid
    , parent : Maybe Uuid
    }


encode : ContractType -> Encode.Value
encode ct =
    Encode.object
        [ ( "what", HType.encode ct.what )
        , ( "uuid", Uuid.encode ct.uuid )
        , ( "parent", Maybe.map Uuid.encode ct.parent |> Maybe.withDefault Encode.null )
        ]


decoder : Decode.Decoder ContractType
decoder =
    Decode.map3 ContractType
        (Decode.field "what" HType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)
