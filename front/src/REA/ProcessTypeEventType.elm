module REA.ProcessTypeEventType exposing (ProcessTypeEventType, compare)

import Json.Decode as Decode exposing (Decoder, andThen)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (toString)
import REA.EventType as ET exposing (EventType)
import REA.ProcessType as PT exposing (ProcessType)
import Time exposing (millisToPosix, posixToMillis)



-- Represent the link between processes and commitments


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
