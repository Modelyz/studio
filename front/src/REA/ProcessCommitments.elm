module REA.ProcessCommitments exposing (ProcessCommitments, compare)

import Prng.Uuid as Uuid exposing (Uuid)



-- Represents the link between processes and commitments


type alias ProcessCommitments =
    { process : Uuid
    , commitment : Uuid
    }


compare : ProcessCommitments -> String
compare pc =
    Uuid.toString pc.process ++ " " ++ Uuid.toString pc.commitment
