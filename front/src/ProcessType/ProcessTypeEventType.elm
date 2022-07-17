module ProcessType.ProcessTypeEventType exposing (ProcessTypeEventType, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)



-- Represent the link between processes and events


type alias ProcessTypeEventType =
    { ptype : Uuid
    , etype : Uuid
    }


compare : ProcessTypeEventType -> String
compare ptet =
    Uuid.toString ptet.ptype ++ "_" ++ Uuid.toString ptet.etype


encode : ProcessTypeEventType -> Encode.Value
encode ptet =
    Encode.object
        [ ( "ptype", Uuid.encode ptet.ptype )
        , ( "etype", Uuid.encode ptet.etype )
        ]


decoder : Decoder ProcessTypeEventType
decoder =
    Decode.map2 ProcessTypeEventType
        (Decode.field "ptype" Uuid.decoder)
        (Decode.field "etype" Uuid.decoder)
