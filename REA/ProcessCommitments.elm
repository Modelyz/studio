module REA.ProcessCommitments exposing (ProcessCommitments, compare)

import Prng.Uuid as Uuid exposing (toString)
import REA.Commitment exposing (Commitment)
import REA.Process exposing (Process)



-- Represent the link between processes and commitments


type alias ProcessCommitments =
    { process : Process
    , commitment : Commitment
    }


compare : ProcessCommitments -> String
compare pc =
    Uuid.toString pc.commitment.uuid
