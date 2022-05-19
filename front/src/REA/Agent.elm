module REA.Agent exposing (Agent, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.AgentType as AT exposing (AgentType)


type alias Agent =
    { name : String
    , type_ : String
    }


encode : Agent -> Encode.Value
encode a =
    Encode.object
        [ ( "name", Encode.string a.name )
        , ( "type", Encode.string a.type_ )
        ]


decoder : Decoder Agent
decoder =
    Decode.map2 Agent
        (Decode.field "name" Decode.string)
        (Decode.field "type" Decode.string)


compare : Agent -> String
compare =
    .name
