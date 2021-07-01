module REA.Commitment exposing (..)

import Json.Encode
import Json.Decode
import REA
import REA.CommitmentType
import Prng.Uuid


new: Prng.Uuid.Uuid -> REA.Commitment
new uuid =
    { name="Pizza order"
    , uuid=uuid
    , ctype=REA.CommitmentType.new uuid
    }


encode : REA.Commitment -> Json.Encode.Value
encode c =
    Json.Encode.object
        [ ("name", Json.Encode.string c.name)
        , ("uuid", Prng.Uuid.encode c.uuid)
        , ("ctype", REA.CommitmentType.encode c.ctype)
        ]


decode : Json.Decode.Decoder REA.Commitment
decode =
    Json.Decode.map3 REA.Commitment
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "uuid" Prng.Uuid.decoder)
        (Json.Decode.field "ctype" REA.CommitmentType.decode)
