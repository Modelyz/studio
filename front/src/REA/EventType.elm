module REA.EventType exposing (EventType, compare, decoder, encode, new)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Maybe exposing (Maybe(..))


type alias EventType =
    { name : String
    , etype : Maybe String
    }


new : String -> EventType
new name =
    { name = name
    , etype = Nothing
    }


encode : EventType -> Encode.Value
encode et =
    Encode.object
        [ ( "name", Encode.string et.name )
        , ( "etype"
          , case et.etype of
                Nothing ->
                    Encode.null

                Just t ->
                    Encode.string t
          )
        ]


decoder : Decoder EventType
decoder =
    Decode.map2 EventType
        (Decode.field "name" Decode.string)
        (Decode.maybe <| Decode.field "etype" Decode.string)


compare : EventType -> String
compare =
    .name
