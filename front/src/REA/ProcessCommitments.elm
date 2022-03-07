module REA.ProcessCommitments exposing (ProcessCommitments, compare)

-- Represent the link between processes and commitments


type alias ProcessCommitments =
    { process : String
    , commitment : String
    }


compare : ProcessCommitments -> String
compare pc =
    pc.process ++ " " ++ pc.commitment
