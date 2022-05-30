module REA.ProcessTypeCommitmentType exposing (ProcessTypeCommitmentType, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import REA.ProcessTypeEventType as PTET



-- Represent the link between processes and commitments


type alias ProcessTypeCommitmentType =
    { ptype : String
    , ctype : String
    }


compare : ProcessTypeCommitmentType -> String
compare ptct =
    ptct.ptype ++ "_" ++ ptct.ctype


encode : ProcessTypeCommitmentType -> Encode.Value
encode ptct =
    Encode.object
        [ ( "ptype", Encode.string ptct.ptype )
        , ( "ctype", Encode.string ptct.ctype )
        ]


decoder : Decoder ProcessTypeCommitmentType
decoder =
    Decode.map2 ProcessTypeCommitmentType
        (Decode.field "ptype" Decode.string)
        (Decode.field "ctype" Decode.string)
