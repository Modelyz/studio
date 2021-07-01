module REA.ContractType exposing (..)

import Json.Decode
import Json.Encode
import Maybe exposing (Maybe(..))

import REA

new : REA.ContractType
new =
    REA.ContractType
    { name="Pizza Sale"
    , ctype=Nothing }


encode : REA.ContractType -> Json.Encode.Value
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


extract (REA.ContractType t) = t


construct : String -> Maybe REA.ContractType -> REA.ContractType
construct name ctype =
    REA.ContractType { name=name, ctype=ctype }


decode : Json.Decode.Decoder REA.ContractType
decode =
    Json.Decode.map2 construct
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "ctype" <| Json.Decode.maybe <| Json.Decode.lazy (\_ -> decode))


entity : REA.ContractType -> Json.Decode.Decoder REA.Entity
entity contractType = Json.Decode.succeed <| REA.CONTRACTTYPE contractType

