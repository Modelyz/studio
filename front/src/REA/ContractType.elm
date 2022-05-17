module REA.ContractType exposing (ContractType, decoder, encode)

import Json.Decode
import Json.Encode
import Maybe exposing (Maybe(..))


type alias ContractType =
    { name : String
    , type_ : Maybe String
    }


encode : ContractType -> Json.Encode.Value
encode ct =
    Json.Encode.object
        [ ( "name", Json.Encode.string ct.name )
        , ( "type", Maybe.map Json.Encode.string ct.type_ |> Maybe.withDefault Json.Encode.null )
        ]


decoder : Json.Decode.Decoder ContractType
decoder =
    Json.Decode.map2 ContractType
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "type" <| Json.Decode.maybe <| Json.Decode.string)
