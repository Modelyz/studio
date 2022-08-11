module Process.Process exposing (Process, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Time exposing (millisToPosix, posixToMillis)
import Type exposing (Type)



-- a process is the wrapper around other entities
-- Processes are independant and unrelated one another
-- if a payment is done for 2 processes, then it's the same process


type alias Process =
    { what : Type
    , uuid : Uuid
    , type_ : Uuid
    , when : Time.Posix
    }


compare : Process -> String
compare =
    .uuid >> Uuid.toString


encode : Process -> Encode.Value
encode p =
    Encode.object
        [ ( "what", Type.encode p.what )
        , ( "type", Uuid.encode p.type_ )
        , ( "uuid", Uuid.encode p.uuid )
        , ( "when", Encode.int <| posixToMillis p.when )
        ]



--merge : Process -> Process -> Process
-- TODO : we start 2 independent sales but the customer pays for both
-- at the same time : then it's the same process
-- Looks like a process can have several contracts?


decoder : Decode.Decoder Process
decoder =
    Decode.map4 Process
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.field "when" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))
