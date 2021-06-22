module REA.ContractType exposing (..)

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
