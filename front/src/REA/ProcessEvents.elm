module REA.ProcessEvents exposing (ProcessEvents, compare)

import Prng.Uuid as Uuid exposing (toString)
import REA.Event exposing (Event)
import REA.Process exposing (Process)
import Time exposing (millisToPosix, posixToMillis)



-- Represent the link between processes and events


type alias ProcessEvents =
    { process : Process
    , event : Event
    }


compare : ProcessEvents -> Int
compare =
    .event >> .posixtime >> posixToMillis
