module Event.Event exposing (Event, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Time exposing (millisToPosix, posixToMillis)
import Typed.Type as TType


type alias Event =
    { what : TType.Type
    , uuid : Uuid
    , type_ : Uuid
    , when : Time.Posix

    --    , qty: Float
    --    , rtype: ResourceType
    --    , provider: Agent
    --    , receiver: Agent
    }


encode : Event -> Encode.Value
encode e =
    Encode.object <|
        [ ( "what", TType.encode e.what )
        , ( "uuid", Uuid.encode e.uuid )
        , ( "type", Uuid.encode e.type_ )
        , ( "when", Encode.int <| posixToMillis e.when )
        ]


decoder : Decoder Event
decoder =
    Decode.map4 Event
        (Decode.field "what" TType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.field "when" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))
