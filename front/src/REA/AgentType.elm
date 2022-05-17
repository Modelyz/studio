module REA.AgentType exposing (AgentType, compare, decoder, encode, toDesc, toString)

import Json.Decode
import Json.Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid


type alias AgentType =
    { name : String
    , type_ : Maybe String
    }


encode : AgentType -> Json.Encode.Value
encode at =
    Json.Encode.object
        [ ( "name", Json.Encode.string at.name )
        , ( "type", Maybe.map Json.Encode.string at.type_ |> Maybe.withDefault Json.Encode.null )
        ]


decoder : Json.Decode.Decoder AgentType
decoder =
    Json.Decode.map2 AgentType
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "type" <| Json.Decode.maybe Json.Decode.string)


compare : AgentType -> String
compare =
    .name


toString : AgentType -> String
toString at =
    at.name


toDesc : AgentType -> Maybe String
toDesc at =
    at.type_
