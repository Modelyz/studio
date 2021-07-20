module REA.Agent exposing (Agent, decoder, encode)

import Json.Decode
import Json.Encode
import Prng.Uuid
import REA.AgentType as AT exposing (AgentType)


type alias Agent =
    { name : String
    , uuid : Prng.Uuid.Uuid
    , atype : AgentType
    }


encode : Agent -> Json.Encode.Value
encode a =
    Json.Encode.object
        [ ( "name", Json.Encode.string a.name )
        , ( "uuid", Prng.Uuid.encode a.uuid )
        , ( "atype", AT.encode a.atype )
        ]


decoder : Json.Decode.Decoder Agent
decoder =
    Json.Decode.map3 Agent
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "uuid" Prng.Uuid.decoder)
        (Json.Decode.field "atype" AT.decoder)
