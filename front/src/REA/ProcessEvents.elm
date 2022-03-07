module REA.ProcessEvents exposing (ProcessEvents, compare)

-- Represent the link between processes and events


type alias ProcessEvents =
    { process : String
    , event : String
    }


compare : ProcessEvents -> String
compare pe =
    pe.process ++ " " ++ pe.event
