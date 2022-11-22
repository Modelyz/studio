module Group.Groupable exposing (Groupable(..), uuid)

import Prng.Uuid exposing (Uuid)
import ResourceType.ResourceType exposing (ResourceType)



-- TODO remove?


type Groupable
    = RT ResourceType


uuid : Groupable -> Uuid
uuid x =
    case x of
        RT rt ->
            rt.uuid
