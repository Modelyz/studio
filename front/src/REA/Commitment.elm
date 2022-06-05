module REA.Commitment exposing (Commitment, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.CommitmentType as CT exposing (CommitmentType)
import Time exposing (millisToPosix, posixToMillis)


type alias Commitment =
    { uuid : Uuid
    , type_ : String
    , when : Time.Posix

    --        , qty: Float
    --        , rtype: ResourceType
    --        , provider: Agent
    --        , receiver: Agent
    }


encode : Commitment -> Encode.Value
encode c =
    Encode.object
        [ ( "uuid", Uuid.encode c.uuid )
        , ( "type", Encode.string c.type_ )
        , ( "when", Encode.int <| posixToMillis c.when )
        ]


decoder : Decode.Decoder Commitment
decoder =
    Decode.map3 Commitment
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Decode.string)
        (Decode.field "when" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))


compare : Commitment -> Int
compare =
    .when >> posixToMillis
