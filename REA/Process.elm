module REA.Process exposing (..)

import Json.Decode
import Json.Encode
import Prng.Uuid

import REA.Commitment as Cm exposing (Commitment)
import REA.Contract as C exposing (Contract)
import REA.Event as E exposing (Event)

-- a process is the wrapper around other entities
-- Processes are independant and unrelated one another
-- if a payment is done for 2 processes, then it's the same process
type alias Process =
        { uuid: Prng.Uuid.Uuid
        , name: String
        , contracts: List Contract
        , commitments: List Commitment
        , events: List Event
        }


new : Prng.Uuid.Uuid -> Process
new uuid =
    { uuid=uuid
    , name="Pizza sale"
    , contracts=[C.new]
    , commitments=[]
    , events=[]
--    , fullfilments=[]
    }

encode : Process -> Json.Encode.Value
encode p =
    Json.Encode.object
        [ ("uuid", Prng.Uuid.encode p.uuid)
        , ("name", Json.Encode.string p.name)
        , ("contracts", Json.Encode.list C.encode p.contracts)
        , ("commitments", Json.Encode.list Cm.encode p.commitments)
        , ("events", Json.Encode.list E.encode p.events)
        ]


--merge : Process -> Process -> Process
-- TODO : we start 2 independent sales but the customer pays for both
-- at the same time : then it's the same process
-- Looks like a process can have several contracts?


decode : Json.Decode.Decoder Process
decode =
    Json.Decode.map5 Process
        (Json.Decode.field "uuid" Prng.Uuid.decoder)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "contracts" <| Json.Decode.list C.decode)
        (Json.Decode.field "commitments" <| Json.Decode.list Cm.decode)
        (Json.Decode.field "events" <| Json.Decode.list E.decode)
