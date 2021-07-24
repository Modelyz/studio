module REA.Commitment exposing (Commitment, compare, decoder, encode, new)

import Json.Decode as Decode
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.CommitmentType as CT exposing (CommitmentType)
import Time exposing (millisToPosix, posixToMillis)


type alias Commitment =
    { name : String
    , uuid : Uuid
    , posixtime : Time.Posix
    , ctype : CommitmentType

    --        , qty: Float
    --        , rtype: ResourceType
    --        , provider: Agent
    --        , receiver: Agent
    }


new : String -> Uuid -> Time.Posix -> CommitmentType -> Commitment
new name uuid posixtime ctype =
    { name = name
    , uuid = uuid
    , posixtime = posixtime
    , ctype = ctype
    }


encode : Commitment -> Encode.Value
encode c =
    Encode.object
        [ ( "name", Encode.string c.name )
        , ( "uuid", Uuid.encode c.uuid )
        , ( "posixtime", Encode.int <| posixToMillis c.posixtime )
        , ( "ctype", CT.encode c.ctype )
        ]


decoder : Decode.Decoder Commitment
decoder =
    Decode.map4 Commitment
        (Decode.field "name" Decode.string)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "posixtime" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))
        (Decode.field "ctype" CT.decoder)


compare : Commitment -> Int
compare commitment =
    posixToMillis commitment.posixtime
