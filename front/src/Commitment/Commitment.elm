module Commitment.Commitment exposing (Commitment, decoder, encode)

import Agent.Agent as Agent exposing (Agent)
import Dict exposing (Dict)
import Expression exposing (Expression)
import Flow exposing (Flow)
import Json.Decode as Decode
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Time exposing (millisToPosix, posixToMillis)
import Typed.Type as TType


type alias Commitment =
    { what : TType.Type
    , uuid : Uuid
    , when : Time.Posix
    , qty : Expression
    , type_ : Uuid
    , provider : Uuid
    , receiver : Uuid
    , flow : Flow
    }


encode : Commitment -> Encode.Value
encode c =
    Encode.object <|
        [ ( "what", TType.encode c.what )
        , ( "uuid", Uuid.encode c.uuid )
        , ( "when", Encode.int <| posixToMillis c.when )
        , ( "qty", Expression.encode c.qty )
        , ( "type", Uuid.encode c.type_ )
        , ( "provider", Uuid.encode c.provider )
        , ( "receiver", Uuid.encode c.receiver )
        , ( "flow", Flow.encode c.flow )
        ]


decoder : Decode.Decoder Commitment
decoder =
    Decode.map8
        (\what uuid type_ when ->
            {- provider receiver flow -}
            Commitment what uuid type_ when
         {- provider receiver flow -}
        )
        (Decode.field "what" TType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "when" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))
        (Decode.field "qty" Expression.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.field "provider" Uuid.decoder)
        (Decode.field "receiver" Uuid.decoder)
        (Decode.field "flow" Flow.decoder)
