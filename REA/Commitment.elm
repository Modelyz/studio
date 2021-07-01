module REA.Commitment exposing (..)

import Json.Encode
import REA
import REA.CommitmentType
import Prng.Uuid


new: Prng.Uuid.Uuid -> REA.Commitment
new uuid =
    { name="Pizza order"
    , uuid=uuid
--    , ctype=REA.CommitmentType.new
    }


encode : REA.Commitment -> Json.Encode.Value
encode c =
    Json.Encode.object
        [ ("name", Json.Encode.string c.name)
        , ("uuid", Prng.Uuid.encode c.uuid)
--        , ("ctype", REA.CommitmentType.encode c.ctype)
        ]

