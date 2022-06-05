module REA.ResourceType exposing (ResourceType, compare, decoder, encode)

import Json.Decode
import Json.Encode


type alias ResourceType =
    { name : String
    , type_ : Maybe String
    }


compare : ResourceType -> String
compare =
    .name


encode : ResourceType -> Json.Encode.Value
encode rt =
    Json.Encode.object
        [ ( "name", Json.Encode.string rt.name )
        , ( "type", Maybe.map Json.Encode.string rt.type_ |> Maybe.withDefault Json.Encode.null )
        ]


decoder : Json.Decode.Decoder ResourceType
decoder =
    Json.Decode.map2 ResourceType
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "type" <| Json.Decode.maybe Json.Decode.string)
