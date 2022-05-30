module REA.Process exposing (Process, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Time exposing (millisToPosix, posixToMillis)



-- a process is the wrapper around other entities
-- Processes are independant and unrelated one another
-- if a payment is done for 2 processes, then it's the same process


type alias Process =
    { type_ : String
    , name : String
    , uuid : Uuid -- TODO rename uuid to producer
    , when : Time.Posix
    }


compare : Process -> Int
compare =
    .when >> posixToMillis


encode : Process -> Encode.Value
encode p =
    Encode.object
        [ ( "name", Encode.string p.name )
        , ( "uuid", Uuid.encode p.uuid )
        , ( "when", Encode.int <| posixToMillis p.when )
        , ( "type", Encode.string p.type_ )
        ]



--merge : Process -> Process -> Process
-- TODO : we start 2 independent sales but the customer pays for both
-- at the same time : then it's the same process
-- Looks like a process can have several contracts?


decoder : Decode.Decoder Process
decoder =
    Decode.map4 Process
        (Decode.field "type" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "when" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))
