module REA.Contract exposing (Contract, compare, decoder, encode)

import Json.Decode
import Json.Encode
import REA.ContractType as CT exposing (ContractType)


type alias Contract =
    { name : String
    , type_ : String

    --    , parties: List Agent
    --    , clauses:
    --    , terms:
    }


encode : Contract -> Json.Encode.Value
encode c =
    Json.Encode.object
        [ ( "name", Json.Encode.string c.name )
        , ( "type", Json.Encode.string c.type_ )

        --        , ("parties", Json.Encode.list Agent.encode c.parties)
        ]


decoder : Json.Decode.Decoder Contract
decoder =
    Json.Decode.map2 Contract
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "type" Json.Decode.string)


compare : Contract -> String
compare =
    .name
