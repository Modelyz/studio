module REA.AgentType exposing (..)

import Maybe exposing (Maybe(..))
import Json.Encode
import REA

encode : REA.AgentType -> Json.Encode.Value
encode at =
    let
        rec = extract at
        t = rec.atype
    in
    Json.Encode.object
        [("name", Json.Encode.string rec.name)
        ,("atype",
            case t of
                Nothing -> Json.Encode.string ""
                Just x -> encode x)
        ]

extract (REA.AgentType t) = t
