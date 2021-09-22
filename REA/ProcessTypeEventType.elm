module REA.ProcessTypeEventType exposing (ProcessTypeEventType, compare)

import Prng.Uuid as Uuid exposing (toString)
import REA.EventType exposing (EventType)
import REA.ProcessType exposing (ProcessType)
import Time exposing (millisToPosix, posixToMillis)



-- Represent the link between processes and commitments


type alias ProcessTypeEventType =
    { ptype : ProcessType
    , etype : EventType
    }


compare : ProcessTypeEventType -> String
compare ptet =
    ptet.ptype.name ++ "_" ++ ptet.etype.name
