module REA.Process exposing (Process, compare, decoder, encode, new)

import Json.Decode as Decode
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Time exposing (millisToPosix, posixToMillis)



-- a process is the wrapper around other entities
-- Processes are independant and unrelated one another
-- if a payment is done for 2 processes, then it's the same process


type alias Process =
    { uuid : Uuid
    , posixtime : Time.Posix
    , name : String
    }


compare : Process -> Int
compare process =
    posixToMillis process.posixtime


new : Uuid -> Time.Posix -> String -> Process
new uuid posixtime ptname =
    { uuid = uuid
    , posixtime = posixtime
    , name = ptname

    --    , fullfilments=[]
    }


encode : Process -> Encode.Value
encode p =
    Encode.object
        [ ( "uuid", Uuid.encode p.uuid )
        , ( "posixtime", Encode.int <| posixToMillis p.posixtime )
        , ( "name", Encode.string p.name )
        ]



--merge : Process -> Process -> Process
-- TODO : we start 2 independent sales but the customer pays for both
-- at the same time : then it's the same process
-- Looks like a process can have several contracts?


decoder : Decode.Decoder Process
decoder =
    Decode.map3 Process
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "posixtime" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))
        (Decode.field "name" Decode.string)
