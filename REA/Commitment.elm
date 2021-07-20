module REA.Commitment exposing (Commitment, compare, decoder, encode, new)

import Json.Decode
import Json.Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.CommitmentType as CT exposing (CommitmentType)


type alias Commitment =
    { name : String
    , uuid : Uuid
    , ctype : CommitmentType

    --        , qty: Float
    --        , rtype: ResourceType
    --        , provider: Agent
    --        , receiver: Agent
    }


new : Uuid -> Commitment
new uuid =
    { name = "Pizza order"
    , uuid = uuid
    , ctype = CT.new uuid
    }


encode : Commitment -> Json.Encode.Value
encode c =
    Json.Encode.object
        [ ( "name", Json.Encode.string c.name )
        , ( "uuid", Uuid.encode c.uuid )
        , ( "ctype", CT.encode c.ctype )
        ]


decoder : Json.Decode.Decoder Commitment
decoder =
    Json.Decode.map3 Commitment
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "uuid" Uuid.decoder)
        (Json.Decode.field "ctype" CT.decoder)


compare : Commitment -> String
compare commitment =
    Uuid.toString commitment.uuid
