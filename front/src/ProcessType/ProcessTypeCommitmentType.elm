module ProcessType.ProcessTypeCommitmentType exposing (ProcessTypeCommitmentType, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)



-- Represent the link between processes and commitments


type alias ProcessTypeCommitmentType =
    { ptype : Uuid
    , ctype : Uuid
    }


compare : ProcessTypeCommitmentType -> String
compare ptct =
    Uuid.toString ptct.ptype ++ "_" ++ Uuid.toString ptct.ctype


encode : ProcessTypeCommitmentType -> Encode.Value
encode ptct =
    Encode.object
        [ ( "ptype", Uuid.encode ptct.ptype )
        , ( "ctype", Uuid.encode ptct.ctype )
        ]


decoder : Decoder ProcessTypeCommitmentType
decoder =
    Decode.map2 ProcessTypeCommitmentType
        (Decode.field "ptype" Uuid.decoder)
        (Decode.field "ctype" Uuid.decoder)
