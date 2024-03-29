module EventType.EventType exposing (EventType, decoder, encode)

import Expression exposing (Expression)
import Hierarchy.Type as HType
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Prng.Uuid as Uuid exposing (Uuid)
import Scope exposing (Scope)


type alias EventType =
    { what : HType.Type
    , uuid : Uuid
    , parent : Maybe Uuid
    , providers : Scope
    , receivers : Scope
    , resources : Scope
    , createResource : Bool
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
        , ( "resources", Scope.encode et.resources )
        , ( "createResource", Encode.bool et.createResource )
        , ( "qty", Expression.encode et.qty )
        ]


decoder : Decode.Decoder EventType
decoder =
    Decode.map8 EventType
        (Decode.field "what" HType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)
        (Decode.field "providers" Scope.decoder)
        (Decode.field "receivers" Scope.decoder)
        (Decode.field "resources" Scope.decoder)
        (Decode.field "createResource" Decode.bool)
        (Decode.field "qty" Expression.decoder)
