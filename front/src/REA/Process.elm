module REA.Process exposing (Process, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Time exposing (millisToPosix, posixToMillis)



-- a process is the wrapper around other entities
-- Processes are independant and unrelated one another
-- if a payment is done for 2 processes, then it's the same process


type alias Process =
    { uuid : Uuid -- TODO rename uuid to producer
    , type_ : String
    , when : Time.Posix
    }


compare : Process -> Int
compare =
    .when >> posixToMillis


encode : Process -> Encode.Value
encode p =
    Encode.object
        [ ( "type", Encode.string p.type_ )
        , ( "uuid", Uuid.encode p.uuid )
        , ( "when", Encode.int <| posixToMillis p.when )
        ]



--merge : Process -> Process -> Process
-- TODO : we start 2 independent sales but the customer pays for both
-- at the same time : then it's the same process
-- Looks like a process can have several contracts?


decoder : Decode.Decoder Process
decoder =
    Decode.map3 Process
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Decode.string)
        (Decode.field "when" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))
