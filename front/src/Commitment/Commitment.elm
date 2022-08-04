module Commitment.Commitment exposing (Commitment, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Time exposing (millisToPosix, posixToMillis)


type alias Commitment =
    { uuid : Uuid
    , type_ : Uuid
    , group : Maybe Uuid
    , when : Time.Posix

    --        , qty: Float
    --        , rtype: ResourceType
    --        , provider: Agent
    --        , receiver: Agent
    }


encode : Commitment -> Encode.Value
encode c =
    Encode.object <|
        [ ( "uuid", Uuid.encode c.uuid )
        , ( "type", Uuid.encode c.type_ )
        , ( "when", Encode.int <| posixToMillis c.when )
        ]
            ++ (Maybe.map (\g -> [ ( "group", Uuid.encode g ) ]) c.group |> Maybe.withDefault [])


decoder : Decode.Decoder Commitment
decoder =
    Decode.map4 Commitment
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.maybe (Decode.field "group" Uuid.decoder))
        (Decode.field "when" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))


compare : Commitment -> Int
compare =
    .when >> posixToMillis
