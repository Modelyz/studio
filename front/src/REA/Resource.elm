module REA.Resource exposing (Resource)

import Prng.Uuid exposing (Uuid)
import REA.ResourceType exposing (ResourceType)


type alias Resource =
    { name : String
    , uuid : Uuid
    , type_ : String
    }
