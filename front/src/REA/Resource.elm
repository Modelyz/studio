module REA.Resource exposing (Resource, compare)

import Prng.Uuid exposing (Uuid)
import REA.ResourceType exposing (ResourceType)


type alias Resource =
    { name : String
    , uuid : Uuid
    , type_ : String
    }


compare : Resource -> String
compare =
    .name
