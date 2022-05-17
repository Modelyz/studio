module REA.Commitment exposing (Commitment, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.CommitmentType as CT exposing (CommitmentType)
import Time exposing (millisToPosix, posixToMillis)


type alias Commitment =
    { name : String
    , uuid : Uuid
    , when : Time.Posix
    , type_ : CommitmentType

    --        , qty: Float
    --        , rtype: ResourceType
    --        , provider: Agent
    --        , receiver: Agent
    }


encode : Commitment -> Encode.Value
encode c =
    Encode.object
        [ ( "name", Encode.string c.name )
        , ( "uuid", Uuid.encode c.uuid )
        , ( "when", Encode.int <| posixToMillis c.when )
        , ( "type", CT.encode c.type_ )
        ]


decoder : Decode.Decoder Commitment
decoder =
    Decode.map4 Commitment
        (Decode.field "name" Decode.string)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "when" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))
        (Decode.field "type" CT.decoder)


compare : Commitment -> Int
compare =
    .when >> posixToMillis
