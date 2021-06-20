module ES exposing (..)

import Prng.Uuid

type alias Event =
    { uuid: Prng.Uuid.Uuid
    --, timestamp: 
    , name: String
    }

