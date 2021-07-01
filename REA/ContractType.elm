module REA.ContractType exposing (..)

import Json.Decode
import Json.Encode
import Maybe exposing (Maybe(..))


type ContractType =
    ContractType
    { name: String
    , ctype: Maybe ContractType
    }


new : ContractType
new =
    ContractType
    { name="Pizza Sale"
    , ctype=Nothing }


encode : ContractType -> Json.Encode.Value
encode ct =
    let rec = extract ct in
    Json.Encode.object
        [ ("name", Json.Encode.string rec.name)
        , ("ctype",
            let t = .ctype <| rec in
            case t of
                Nothing -> Json.Encode.string ""
                Just x -> encode x
          )
        ]


extract (ContractType t) = t


construct : String -> Maybe ContractType -> ContractType
construct name ctype =
    ContractType { name=name, ctype=ctype }


decode : Json.Decode.Decoder ContractType
decode =
    Json.Decode.map2 construct
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "ctype" <| Json.Decode.maybe <| Json.Decode.lazy (\_ -> decode))
