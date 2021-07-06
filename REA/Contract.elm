module REA.Contract exposing (Contract, decode, encode, new)

import Json.Decode
import Json.Encode
import REA.ContractType as CT exposing (ContractType)


type alias Contract =
    { name : String
    , ctype : ContractType

    --    , parties: List Agent
    --    , clauses:
    --    , terms:
    }


new : Contract
new =
    { name = "Pizza sale"
    , ctype = CT.new

    --    , parties=[]
    }


encode : Contract -> Json.Encode.Value
encode c =
    Json.Encode.object
        [ ( "name", Json.Encode.string c.name )
        , ( "ctype", CT.encode c.ctype )

        --        , ("parties", Json.Encode.list Agent.encode c.parties)
        ]


decode : Json.Decode.Decoder Contract
decode =
    Json.Decode.map2 Contract
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "ctype" CT.decode)
