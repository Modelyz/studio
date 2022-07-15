module Contract.Contract exposing (Contract, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)


type alias Contract =
    { uuid : Uuid
    , type_ : String

    --    , parties: List Agent
    --    , clauses:
    --    , terms:
    }


encode : Contract -> Encode.Value
encode c =
    Encode.object
        [ ( "uuid", Uuid.encode c.uuid )
        , ( "type", Encode.string c.type_ )

        --        , ("parties", Encode.list Agent.encode c.parties)
        ]


decoder : Decode.Decoder Contract
decoder =
    Decode.map2 Contract
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Decode.string)


compare : Contract -> String
compare =
    .uuid >> Uuid.toString
