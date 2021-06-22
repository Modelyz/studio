module REA.Contract exposing (..)

import Json.Encode

import REA 
import REA.Agent
import REA.ContractType

new: REA.Contract
new=
    { name="Pizza sale"
    , ctype=REA.ContractType.new
    , parties=[]
    }

encode : REA.Contract -> Json.Encode.Value
encode c =
    Json.Encode.object
        [ ("name", Json.Encode.string c.name)
        , ("ctype", REA.ContractType.encode c.ctype)
        , ("parties", Json.Encode.list REA.Agent.encode c.parties)
        ]
