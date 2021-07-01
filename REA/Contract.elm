module REA.Contract exposing (..)

import Json.Decode
import Json.Encode

import REA 
import REA.Agent
import REA.ContractType

new: REA.Contract
new=
    { name="Pizza sale"
    , ctype=REA.ContractType.new
--    , parties=[]
    }

encode : REA.Contract -> Json.Encode.Value
encode c =
    Json.Encode.object
        [ ("name", Json.Encode.string c.name)
        , ("ctype", REA.ContractType.encode c.ctype)
--        , ("parties", Json.Encode.list REA.Agent.encode c.parties)
        ]


decode : Json.Decode.Decoder REA.Contract
decode =
    Json.Decode.map2 REA.Contract
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "ctype" REA.ContractType.decode)

        
entity : REA.Contract -> Json.Decode.Decoder REA.Entity
entity contract = Json.Decode.succeed <| REA.CONTRACT contract

