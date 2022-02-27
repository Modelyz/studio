module REA.ProcessType exposing (ProcessType, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- process type is the rea pattern


type alias ProcessType =
    { name : String }


encode : ProcessType -> Encode.Value
encode pt =
    Encode.object
        [ ( "name", Encode.string pt.name )
        ]


decoder : Decoder ProcessType
decoder =
    Decode.map ProcessType
        (Decode.field "name" Decode.string)


compare : ProcessType -> String
compare =
    .name
