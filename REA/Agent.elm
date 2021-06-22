module REA.Agent exposing (..)

import Json.Encode
import REA
import REA.AgentType


encode : REA.Agent -> Json.Encode.Value
encode a =
    Json.Encode.object
        [ ("name", Json.Encode.string a.name)
        , ("atype", REA.AgentType.encode a.atype)
        ]
