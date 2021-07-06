module REA.Commitment exposing (Commitment, decode, encode, new)

import Json.Decode
import Json.Encode
import Prng.Uuid
import REA.CommitmentType as CT exposing (CommitmentType)


type alias Commitment =
    { name : String
    , uuid : Prng.Uuid.Uuid
    , ctype : CommitmentType

    --        , qty: Float
    --        , rtype: ResourceType
    --        , provider: Agent
    --        , receiver: Agent
    }


new : Prng.Uuid.Uuid -> Commitment
new uuid =
    { name = "Pizza order"
    , uuid = uuid
    , ctype = CT.new uuid
    }


encode : Commitment -> Json.Encode.Value
encode c =
    Json.Encode.object
        [ ( "name", Json.Encode.string c.name )
        , ( "uuid", Prng.Uuid.encode c.uuid )
        , ( "ctype", CT.encode c.ctype )
        ]


decode : Json.Decode.Decoder Commitment
decode =
    Json.Decode.map3 Commitment
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "uuid" Prng.Uuid.decoder)
        (Json.Decode.field "ctype" CT.decode)
