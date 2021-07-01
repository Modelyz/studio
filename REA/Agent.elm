module REA.Agent exposing (..)

import Json.Decode
import Json.Encode
import Prng.Uuid
import REA
import REA.AgentType

encode : REA.Agent -> Json.Encode.Value
encode a =
    Json.Encode.object
        [ ("name", Json.Encode.string a.name)
        , ("uuid", Prng.Uuid.encode a.uuid)
        , ("atype", REA.AgentType.encode a.atype)
        ]

construct : String -> Prng.Uuid.Uuid -> REA.AgentType -> REA.Agent
construct name uuid atype =
    {name=name, uuid=uuid, atype=atype}


toEntity : REA.Agent -> REA.Entity
toEntity agent = REA.AGENT agent


decode : Json.Decode.Decoder REA.Agent
decode =
    Json.Decode.map3 REA.Agent
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "uuid" Prng.Uuid.decoder)
        (Json.Decode.field "atype" REA.AgentType.decode)
        
entity : REA.Agent -> Json.Decode.Decoder REA.Entity
entity agent = Json.Decode.succeed <| REA.AGENT agent

