module Agent.Agent exposing (Agent, decoder, encode, providerOf, providersOf, receiverOf, receiversOf)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Scope exposing (Scope(..))
import Scope.State as Scope
import Type exposing (Type)
import Typed.Type as TType


type alias Agent =
    { what : TType.Type
    , uuid : Uuid
    , type_ : Uuid
    }


providersOf : Dict String ( Uuid, Type, Maybe Uuid ) -> Dict String { a | providers : Scope } -> Dict String Agent -> Uuid -> List Uuid
providersOf types entities agents uuid =
    Dict.get (Uuid.toString uuid) entities
        |> Maybe.map
            (\e ->
                agents
                    |> Dict.filter (\_ v -> Scope.containsScope types (IsItem (Type.TType TType.Agent) v.uuid) e.providers)
                    |> Dict.values
                    |> List.map .uuid
            )
        |> Maybe.withDefault []


receiversOf : Dict String ( Uuid, Type, Maybe Uuid ) -> Dict String { a | receivers : Scope } -> Dict String Agent -> Uuid -> List Uuid
receiversOf types entities agents uuid =
    Dict.get (Uuid.toString uuid) entities
        |> Maybe.map
            (\e ->
                agents
                    |> Dict.filter (\_ v -> Scope.containsScope types (IsItem (Type.TType TType.Agent) v.uuid) e.receivers)
                    |> Dict.values
                    |> List.map .uuid
            )
        |> Maybe.withDefault []


providerOf : Dict String { a | provider : Uuid } -> Dict String Agent -> Uuid -> List Uuid
providerOf entities agents uuid =
    Dict.get (Uuid.toString uuid) entities
        |> Maybe.map
            (\e ->
                agents
                    |> Dict.filter (\_ v -> v.uuid == e.provider)
                    |> Dict.values
                    |> List.map .uuid
            )
        |> Maybe.withDefault []


receiverOf : Dict String { a | receiver : Uuid } -> Dict String Agent -> Uuid -> List Uuid
receiverOf entities agents uuid =
    Dict.get (Uuid.toString uuid) entities
        |> Maybe.map
            (\e ->
                agents
                    |> Dict.filter (\_ v -> v.uuid == e.receiver)
                    |> Dict.values
                    |> List.map .uuid
            )
        |> Maybe.withDefault []


encode : Agent -> Encode.Value
encode a =
    Encode.object <|
        [ ( "what", TType.encode a.what )
        , ( "uuid", Uuid.encode a.uuid )
        , ( "type_", Uuid.encode a.type_ )
        ]


decoder : Decoder Agent
decoder =
    Decode.map3 Agent
        (Decode.field "what" TType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type_" Uuid.decoder)
