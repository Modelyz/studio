module Event.Event exposing (Event, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Time exposing (millisToPosix, posixToMillis)
import Type exposing (Type)


type alias Event =
    { what : Type
    , uuid : Uuid
    , type_ : Uuid
    , group : Maybe Uuid
    , when : Time.Posix

    --    , qty: Float
    --    , rtype: ResourceType
    --    , provider: Agent
    --    , receiver: Agent
    }


encode : Event -> Encode.Value
encode e =
    Encode.object <|
        [ ( "what", Type.encode e.what )
        , ( "uuid", Uuid.encode e.uuid )
        , ( "type", Uuid.encode e.type_ )
        , ( "when", Encode.int <| posixToMillis e.when )
        ]
            ++ (Maybe.map (\g -> [ ( "group", Uuid.encode g ) ]) e.group |> Maybe.withDefault [])


decoder : Decoder Event
decoder =
    Decode.map5 Event
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.maybe (Decode.field "group" Uuid.decoder))
        (Decode.field "when" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))


compare : Event -> String
compare =
    .uuid >> Uuid.toString
