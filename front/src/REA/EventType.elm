module REA.EventType exposing (EventType, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Maybe exposing (Maybe(..))


type alias EventType =
    { name : String
    , type_ : Maybe String
    }


encode : EventType -> Encode.Value
encode et =
    Encode.object
        [ ( "name", Encode.string et.name )
        , ( "type", Maybe.map Encode.string et.type_ |> Maybe.withDefault Encode.null )
        ]


decoder : Decoder EventType
decoder =
    Decode.map2 EventType
        (Decode.field "name" Decode.string)
        (Decode.maybe <| Decode.field "type" Decode.string)


compare : EventType -> String
compare =
    .name
