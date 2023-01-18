module CommitmentType.CommitmentType exposing (CommitmentType, decoder, encode)

import Expression as Expression exposing (Expression)
import Hierarchy.Type as HType
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Prng.Uuid as Uuid exposing (Uuid)
import Scope as Scope exposing (Scope)
import Type exposing (Type)
import Value.Value exposing (Value)


type alias CommitmentType =
    { what : HType.Type
    , uuid : Uuid
    , parent : Maybe Uuid
    , providers : Scope
    , receivers : Scope
    , flowscope : Scope
    , qty : Expression
    }


encode : CommitmentType -> Encode.Value
encode ct =
    Encode.object <|
        [ ( "what", HType.encode ct.what )
        , ( "uuid", Uuid.encode ct.uuid )
        , ( "parent", Maybe.map Uuid.encode ct.parent |> Maybe.withDefault Encode.null )
        , ( "providers", Scope.encode ct.providers )
        , ( "receivers", Scope.encode ct.receivers )
        , ( "flowscope", Scope.encode ct.flowscope )
        , ( "qty", Expression.encode ct.qty )
        ]


compare : CommitmentType -> String
compare =
    toString


toString : CommitmentType -> String
toString =
    .uuid >> Uuid.toString


decoder : Decode.Decoder CommitmentType
decoder =
    Decode.map7 CommitmentType
        (Decode.field "what" HType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)
        (Decode.field "providers" Scope.decoder)
        (Decode.field "receivers" Scope.decoder)
        (Decode.field "flowscope" Scope.decoder)
        (Decode.field "qty" Expression.decoder)
