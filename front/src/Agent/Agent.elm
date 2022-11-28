module Agent.Agent exposing (Agent, decoder, encode, providerOf, receiverOf)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Typed.Type as TType


type alias Agent =
    { what : TType.Type
    , uuid : Uuid
    , type_ : Uuid
    }


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
        , ( "type", Uuid.encode a.type_ )
        ]


decoder : Decoder Agent
decoder =
    Decode.map3 Agent
        (Decode.field "what" TType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
