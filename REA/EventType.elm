module REA.EventType exposing (..)

import Maybe exposing (Maybe(..))
import Json.Encode
import Prng.Uuid
import REA


new: Prng.Uuid.Uuid -> REA.EventType
new uuid=
    REA.EventType
    { name="Sale"
    , uuid=uuid
    , etype=Nothing
    }


encode : REA.EventType -> Json.Encode.Value
encode et =
    let
        rec = extract et
        t = rec.etype
    in
    Json.Encode.object
        [ ("name", Json.Encode.string rec.name)
        , ("uuid", Json.Encode.string <| Prng.Uuid.toString rec.uuid)
        , ("etype",
            case t of
                Nothing -> Json.Encode.string ""
                Just x -> encode x)
        ]

extract (REA.EventType t) = t


