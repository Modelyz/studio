module REA.ProcessType exposing (ProcessType, decoder, encode, new)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- process type is the rea pattern


type alias ProcessType =
    { processName : String }


new : ProcessType
new =
    { processName = "Sale" }


encode : ProcessType -> Encode.Value
encode pt =
    Encode.object
        [ ( "processName", Encode.string pt.processName )
        ]


decoder : Decoder ProcessType
decoder =
    Decode.map ProcessType
        (Decode.field "processName" Decode.string)


compare : ProcessType -> String
compare pt =
    pt.processName
