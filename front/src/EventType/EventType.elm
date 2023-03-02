module EventType.EventType exposing (EventType, decoder, encode)

import Expression as Expression exposing (Expression)
import Hierarchy.Type as HType
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Prng.Uuid as Uuid exposing (Uuid)
import Scope as Scope exposing (Scope)


type alias EventType =
    { what : HType.Type
    , uuid : Uuid
    , parent : Maybe Uuid
    , providers : Scope
    , receivers : Scope
    , flowscope : Scope
    , qty : Expression
    }


encode : EventType -> Encode.Value
encode et =
    Encode.object <|
        [ ( "what", HType.encode et.what )
        , ( "uuid", Uuid.encode et.uuid )
        , ( "parent", Maybe.map Uuid.encode et.parent |> Maybe.withDefault Encode.null )
        , ( "providers", Scope.encode et.providers )
        , ( "receivers", Scope.encode et.receivers )
        , ( "flowscope", Scope.encode et.flowscope )
        , ( "qty", Expression.encode et.qty )
        ]


decoder : Decode.Decoder EventType
decoder =
    Decode.map7 EventType
        (Decode.field "what" HType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)
        (Decode.field "providers" Scope.decoder)
        (Decode.field "receivers" Scope.decoder)
        (Decode.field "flowscope" Scope.decoder)
        (Decode.field "qty" Expression.decoder)
