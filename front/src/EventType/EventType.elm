module EventType.EventType exposing (EventType, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid as Uuid exposing (Uuid)


type alias EventType =
    { uuid : Uuid
    , type_ : Maybe Uuid
    }


encode : EventType -> Encode.Value
encode at =
    Encode.object
        [ ( "uuid", Uuid.encode at.uuid )
        , ( "type", Maybe.map Uuid.encode at.type_ |> Maybe.withDefault Encode.null )
        ]


decoder : Decode.Decoder EventType
decoder =
    Decode.map2 EventType
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" <| Decode.maybe Uuid.decoder)


compare : EventType -> String
compare =
    toString


toString : EventType -> String
toString =
    .uuid >> Uuid.toString
