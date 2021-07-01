module ES exposing (..)

import Prng.Uuid
import Json.Encode
import Time
import REA
import REA.Entity

type alias Event =
    { uuid: Prng.Uuid.Uuid
    , posixtime: Time.Posix
    , name: String
    , entity: REA.Entity
    , entityType: String
    }


encode : Event -> Json.Encode.Value
encode event =
    Json.Encode.object
        [ ("uuid", Prng.Uuid.encode event.uuid)
        , ("posixtime", Json.Encode.int <| Time.posixToMillis event.posixtime)
        , ("name", Json.Encode.string event.name)
        , ("entityType", Json.Encode.string event.entityType)
        , ("entity", REA.Entity.encode event.entity)
        ]

