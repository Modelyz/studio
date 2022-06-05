module REA.Event exposing (Event, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Time exposing (millisToPosix, posixToMillis)


type alias Event =
    { uuid : Uuid
    , type_ : String
    , when : Time.Posix

    --    , qty: Float
    --    , rtype: ResourceType
    --    , provider: Agent
    --    , receiver: Agent
    }


encode : Event -> Encode.Value
encode e =
    Encode.object
        [ ( "uuid", Uuid.encode e.uuid )
        , ( "type", Encode.string e.type_ )
        , ( "when", Encode.int <| posixToMillis e.when )
        ]


decoder : Decoder Event
decoder =
    Decode.map3 Event
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Decode.string)
        (Decode.field "when" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))


compare : Event -> Int
compare =
    .when >> posixToMillis
