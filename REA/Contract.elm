module REA.Contract exposing (Contract, decoder, encode, new)

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


new : String -> Contract
new cname =
    { name = cname
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


decoder : Json.Decode.Decoder Contract
decoder =
    Json.Decode.map2 Contract
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "ctype" CT.decoder)
