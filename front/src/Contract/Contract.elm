module Contract.Contract exposing (Contract, compare, decoder, encode)

import Dict exposing (Dict)
import Group.Group exposing (Group)
import Ident.Identifier exposing (Identifier)
import Json.Decode as Decode
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)
import Value.Value exposing (Value)


type alias Contract =
    { what : Type
    , uuid : Uuid
    , type_ : Uuid
    , identifiers : Dict String Identifier
    , values : Dict String Value
    , groups : Dict String Group
    , display : Dict String String

    --    , parties: List Agent
    --    , clauses:
    --    , terms:
    }


encode : Contract -> Encode.Value
encode c =
    Encode.object
        [ ( "what", Type.encode c.what )
        , ( "uuid", Uuid.encode c.uuid )
        , ( "type", Uuid.encode c.type_ )

        --        , ("parties", Encode.list Agent.encode c.parties)
        ]


decoder : Decode.Decoder Contract
decoder =
    Decode.map7 Contract
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)
        (Decode.succeed Dict.empty)


compare : Contract -> String
compare =
    .uuid >> Uuid.toString
