module Relation.ProcessEvents exposing (ProcessEvents, compare)

import Prng.Uuid as Uuid exposing (Uuid)



-- Represents the link between processes and events


type alias ProcessEvents =
    { process : Uuid
    , event : Uuid
    }


compare : ProcessEvents -> String
compare pe =
    Uuid.toString pe.process ++ " " ++ Uuid.toString pe.event
