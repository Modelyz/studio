module REA.Event exposing (..)

import Json.Encode
import REA
import REA.EventType
import Prng.Uuid


new: Prng.Uuid.Uuid -> REA.Event
new uuid=
    { name="Pizza order"
    , uuid=uuid
    , etype=REA.EventType.new uuid -- FIXME select the type
    }


encode : REA.Event -> Json.Encode.Value
encode e =
    Json.Encode.object
        [ ("name", Json.Encode.string e.name)
        , ("uuid", Json.Encode.string <| Prng.Uuid.toString e.uuid)
        , ("etype", REA.EventType.encode e.etype)
        ]


