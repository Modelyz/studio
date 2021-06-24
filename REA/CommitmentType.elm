module REA.CommitmentType exposing (..)

import Maybe exposing (Maybe(..))
import Json.Encode
import REA
import Prng.Uuid


new : Prng.Uuid.Uuid -> REA.CommitmentType
new uuid=
    REA.CommitmentType
    { name="Order"
    , uuid=uuid
    , ctype=Nothing
    }


encode : REA.CommitmentType -> Json.Encode.Value
encode ct =
    let
        rec = extract ct
        t = rec.ctype
    in
    Json.Encode.object
        [ ("name", Json.Encode.string rec.name)
        , ("uuid", Json.Encode.string <| Prng.Uuid.toString rec.uuid)
        , ("ctype",
            case t of
                Nothing -> Json.Encode.string ""
                Just x -> encode x)
        ]

extract (REA.CommitmentType t) = t

