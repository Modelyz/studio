module REA.ProcessTypeCommitmentType exposing (ProcessTypeCommitmentType, compare)

-- Represent the link between processes and commitments


type alias ProcessTypeCommitmentType =
    { ptype : String
    , ctype : String
    }


compare : ProcessTypeCommitmentType -> String
compare ptct =
    ptct.ptype ++ "_" ++ ptct.ctype
