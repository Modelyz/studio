module REA.CommitmentType exposing (CommitmentType, compare, decoder, encode, new)

import Json.Decode as Decode exposing (Decoder, andThen)
import Json.Encode as Encode
import Maybe exposing (Maybe(..))


type alias CommitmentType =
    { name : String
    , ctype : Maybe String
    }


new : String -> CommitmentType
new name =
    { name = name
    , ctype = Nothing
    }


encode : CommitmentType -> Encode.Value
encode ct =
    Encode.object
        [ ( "name", Encode.string ct.name )
        , ( "ctype", Encode.string ct.name )
        ]


decoder : Decoder CommitmentType
decoder =
    Decode.map2 CommitmentType
        (Decode.field "name" Decode.string)
        (Decode.maybe <| Decode.field "ctype" Decode.string)


compare : CommitmentType -> String
compare =
    .name
