module REA.ContractType exposing (ContractType, decoder, encode, new)

import Json.Decode
import Json.Encode
import Maybe exposing (Maybe(..))


type ContractType
    = ContractType
        { name : String
        , ctype : Maybe ContractType
        }


new : String -> ContractType
new ctname =
    ContractType
        { name = ctname
        , ctype = Nothing
        }


encode : ContractType -> Json.Encode.Value
encode ct =
    let
        rec =
            extract ct
    in
    Json.Encode.object
        [ ( "name", Json.Encode.string rec.name )
        , ( "ctype"
          , let
                t =
                    .ctype <| rec
            in
            case t of
                Nothing ->
                    Json.Encode.string ""

                Just x ->
                    encode x
          )
        ]


extract : ContractType -> { name : String, ctype : Maybe ContractType }
extract (ContractType t) =
    t


construct : String -> Maybe ContractType -> ContractType
construct name ctype =
    ContractType { name = name, ctype = ctype }


decoder : Json.Decode.Decoder ContractType
decoder =
    Json.Decode.map2 construct
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "ctype" <| Json.Decode.maybe <| Json.Decode.lazy (\_ -> decoder))
