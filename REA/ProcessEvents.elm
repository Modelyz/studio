module REA.ProcessEvents exposing (ProcessEvents, compare)

import Prng.Uuid as Uuid exposing (toString)
import REA.Event exposing (Event)
import REA.Process exposing (Process)



-- Represent the link between processes and events


type alias ProcessEvents =
    { process : Process
    , event : Event
    }


compare : ProcessEvents -> String
compare pc =
    Uuid.toString pc.event.uuid
