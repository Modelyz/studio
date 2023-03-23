module Process.Process exposing (Process, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Typed.Type as TType



-- a process is the wrapper around related partial events
-- Processes are independant and unrelated one another


type alias Process =
    { what : TType.Type
    , uuid : Uuid
    , type_ : Uuid
    }


encode : Process -> Encode.Value
encode p =
    Encode.object
        [ ( "what", TType.encode p.what )
        , ( "type", Uuid.encode p.type_ )
        , ( "uuid", Uuid.encode p.uuid )
        ]



--merge : Process -> Process -> Process
-- TODO : we start 2 independent sales but the customer pays for both
-- at the same time : then it's the same process
-- Looks like a process can have several contracts?


decoder : Decode.Decoder Process
decoder =
    Decode.map3 Process
        (Decode.field "what" TType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)


compare : Process -> String
compare =
    Uuid.toString << .uuid
