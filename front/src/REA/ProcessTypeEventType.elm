module REA.ProcessTypeEventType exposing (ProcessTypeEventType, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- Represent the link between processes and events


type alias ProcessTypeEventType =
    { ptype : String
    , etype : String
    }


compare : ProcessTypeEventType -> String
compare ptet =
    ptet.ptype ++ "_" ++ ptet.etype


encode : ProcessTypeEventType -> Encode.Value
encode ptet =
    Encode.object
        [ ( "ptype", Encode.string ptet.ptype )
        , ( "etype", Encode.string ptet.etype )
        ]


decoder : Decoder ProcessTypeEventType
decoder =
    Decode.map2 ProcessTypeEventType
        (Decode.field "ptype" Decode.string)
        (Decode.field "etype" Decode.string)
