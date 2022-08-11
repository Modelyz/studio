module EventType.EventType exposing (EventType, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type alias EventType =
    { what : Type
    , uuid : Uuid
    , parent : Maybe Uuid
    , group : Maybe Uuid
    }


encode : EventType -> Encode.Value
encode et =
    Encode.object <|
        [ ( "what", Type.encode et.what )
        , ( "uuid", Uuid.encode et.uuid )
        , ( "parent", Maybe.map Uuid.encode et.parent |> Maybe.withDefault Encode.null )
        ]
            ++ (Maybe.map (\g -> [ ( "group", Uuid.encode g ) ]) et.group |> Maybe.withDefault [])


decoder : Decode.Decoder EventType
decoder =
    Decode.map4 EventType
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)
        (Decode.maybe (Decode.field "group" Uuid.decoder))


compare : EventType -> String
compare =
    toString


toString : EventType -> String
toString =
    .uuid >> Uuid.toString
