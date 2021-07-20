module REA.Process exposing (Process, compare, decoder, encode, new)

import Json.Decode
import Json.Encode
import Prng.Uuid as Uuid exposing (Uuid)



-- a process is the wrapper around other entities
-- Processes are independant and unrelated one another
-- if a payment is done for 2 processes, then it's the same process


type alias Process =
    { uuid : Uuid
    , name : String
    }


compare : Process -> String
compare process =
    Uuid.toString process.uuid


new : Uuid -> Process
new uuid =
    { uuid = uuid
    , name = "Pizza sale"

    --    , fullfilments=[]
    }


encode : Process -> Json.Encode.Value
encode p =
    Json.Encode.object
        [ ( "uuid", Uuid.encode p.uuid )
        , ( "name", Json.Encode.string p.name )
        ]



--merge : Process -> Process -> Process
-- TODO : we start 2 independent sales but the customer pays for both
-- at the same time : then it's the same process
-- Looks like a process can have several contracts?


decoder : Json.Decode.Decoder Process
decoder =
    Json.Decode.map2 Process
        (Json.Decode.field "uuid" Uuid.decoder)
        (Json.Decode.field "name" Json.Decode.string)
