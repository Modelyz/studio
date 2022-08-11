module Commitment.Commitment exposing (Commitment, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Time exposing (millisToPosix, posixToMillis)
import Type exposing (Type)


type alias Commitment =
    { what : Type
    , uuid : Uuid
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
        [ ( "what", Type.encode c.what )
        , ( "uuid", Uuid.encode c.uuid )
        , ( "type", Uuid.encode c.type_ )
        , ( "when", Encode.int <| posixToMillis c.when )
        ]
            ++ (Maybe.map (\g -> [ ( "group", Uuid.encode g ) ]) c.group |> Maybe.withDefault [])


decoder : Decode.Decoder Commitment
decoder =
    Decode.map5 Commitment
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.maybe (Decode.field "group" Uuid.decoder))
        (Decode.field "when" Decode.int |> Decode.andThen (\t -> Decode.succeed (millisToPosix t)))


compare : Commitment -> String
compare =
    .uuid >> Uuid.toString
