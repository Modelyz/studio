module REA.Agent exposing (Agent, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.AgentType as AT exposing (AgentType)


type alias Agent =
    { name : Uuid
    , type_ : String
    }


encode : Agent -> Encode.Value
encode a =
    Encode.object
        [ ( "name", Uuid.encode a.name )
        , ( "type", Encode.string a.type_ )
        ]


decoder : Decoder Agent
decoder =
    Decode.map2 Agent
        (Decode.field "name" Uuid.decoder)
        (Decode.field "type" Decode.string)


compare : Agent -> String
compare =
    .name >> Uuid.toString
