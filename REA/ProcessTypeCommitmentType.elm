module REA.ProcessTypeCommitmentType exposing (ProcessTypeCommitmentType, compare)

import Prng.Uuid as Uuid exposing (toString)
import REA.CommitmentType exposing (CommitmentType)
import REA.ProcessType exposing (ProcessType)
import Time exposing (millisToPosix, posixToMillis)



-- Represent the link between processes and commitments


type alias ProcessTypeCommitmentType =
    { ptype : ProcessType
    , ctype : CommitmentType
    }


compare : ProcessTypeCommitmentType -> String
compare ptct =
    ptct.ptype.name ++ "_" ++ ptct.ctype.name
