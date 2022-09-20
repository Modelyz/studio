module Relation.ProcessCommitments exposing (ProcessCommitments)

import Prng.Uuid exposing (Uuid)



-- Represents the link between processes and commitments


type alias ProcessCommitments =
    { process : Uuid
    , commitment : Uuid
    }
