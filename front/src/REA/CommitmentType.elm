module REA.CommitmentType exposing (CommitmentType, compare, decoder, encode, new)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Maybe exposing (Maybe(..))


type alias CommitmentType =
    { name : String
    , type_ : Maybe String
    }


new : String -> CommitmentType
new name =
    { name = name
    , type_ = Nothing
    }


encode : CommitmentType -> Encode.Value
encode ct =
    Encode.object
        [ ( "name", Encode.string ct.name )
        , ( "type", Encode.string ct.name )
        ]


decoder : Decoder CommitmentType
decoder =
    Decode.map2 CommitmentType
        (Decode.field "name" Decode.string)
        (Decode.maybe <| Decode.field "type" Decode.string)


compare : CommitmentType -> String
compare =
    .name
