module Relation.ProcessEvents exposing (ProcessEvents)

import Prng.Uuid exposing (Uuid)



-- Represents the link between processes and events


type alias ProcessEvents =
    { process : Uuid
    , event : Uuid
    }
