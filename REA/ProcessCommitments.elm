module REA.ProcessCommitments exposing (ProcessCommitments, compare)

import Prng.Uuid as Uuid exposing (toString)
import REA.Commitment exposing (Commitment)
import REA.Process exposing (Process)
import Time exposing (millisToPosix, posixToMillis)



-- Represent the link between processes and commitments


type alias ProcessCommitments =
    { process : Process
    , commitment : Commitment
    }


compare : ProcessCommitments -> Int
compare pc =
    posixToMillis pc.commitment.posixtime
