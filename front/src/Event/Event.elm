module Event.Event exposing (Event, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Time exposing (millisToPosix, posixToMillis)
import Typed.Type as TType


type alias Event =
    { what : TType.Type
    , uuid : Uuid
    , when : Time.Posix
    , type_ : Uuid
    , provider : Uuid
    , receiver : Uuid
    , resource : Uuid
    }


encode : Event -> Encode.Value
encode e =
    Encode.object <|
        [ ( "what", TType.encode e.what )
        , ( "uuid", Uuid.encode e.uuid )
        , ( "when", Encode.int <| posixToMillis e.when )
        , ( "type_", Uuid.encode e.type_ )
        , ( "provider", Uuid.encode e.provider )
        , ( "receiver", Uuid.encode e.receiver )
        , ( "resource", Uuid.encode e.resource )
        ]


decoder : Decoder Event
decoder =
    Decode.map7 Event
        (Decode.field "what" TType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "when" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))
        (Decode.field "type_" Uuid.decoder)
        (Decode.field "provider" Uuid.decoder)
        (Decode.field "receiver" Uuid.decoder)
        (Decode.field "resource" Uuid.decoder)
