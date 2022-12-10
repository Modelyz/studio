module Event.Event exposing (Event, decoder, encode)

import Expression exposing (Expression)
import Flow exposing (Flow)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Time exposing (millisToPosix, posixToMillis)
import Typed.Type as TType


type alias Event =
    { what : TType.Type
    , uuid : Uuid
    , when : Time.Posix
    , qty : Expression
    , type_ : Uuid
    , provider : Uuid
    , receiver : Uuid
    , flow : Flow
    }


encode : Event -> Encode.Value
encode e =
    Encode.object <|
        [ ( "what", TType.encode e.what )
        , ( "uuid", Uuid.encode e.uuid )
        , ( "when", Encode.int <| posixToMillis e.when )
        , ( "qty", Expression.encode e.qty )
        , ( "type", Uuid.encode e.type_ )
        , ( "provider", Uuid.encode e.provider )
        , ( "receiver", Uuid.encode e.receiver )
        , ( "flow", Flow.encode e.flow )
        ]


decoder : Decoder Event
decoder =
    Decode.map8 Event
        (Decode.field "what" TType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "when" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))
        (Decode.field "qty" Expression.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.field "provider" Uuid.decoder)
        (Decode.field "receiver" Uuid.decoder)
        (Decode.field "flow" Flow.decoder)
