module REA.AgentType exposing (..)

import Maybe exposing (Maybe(..))
import Json.Encode
import Json.Decode
import Maybe exposing (Maybe(..))
import REA
import Prng.Uuid

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


construct : String -> Prng.Uuid.Uuid -> Maybe REA.AgentType -> REA.AgentType
construct name uuid atype =
    REA.AgentType { name=name, uuid=uuid, atype=atype }


decode : Json.Decode.Decoder REA.AgentType
decode =
    Json.Decode.map3 construct
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "uuid" Prng.Uuid.decoder)
        (Json.Decode.field "atype" <| Json.Decode.maybe <| Json.Decode.lazy (\_ -> decode))

