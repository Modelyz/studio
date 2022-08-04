module Agent.Agent exposing (Agent, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)


type alias Agent =
    { uuid : Uuid
    , type_ : Uuid
    , group : Maybe Uuid
    }


encode : Agent -> Encode.Value
encode a =
    Encode.object <|
        [ ( "uuid", Uuid.encode a.uuid )
        , ( "type", Uuid.encode a.type_ )
        ]
            ++ (Maybe.map (\g -> [ ( "group", Uuid.encode g ) ]) a.group |> Maybe.withDefault [])


decoder : Decoder Agent
decoder =
    Decode.map3 Agent
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.maybe (Decode.field "group" Uuid.decoder))


compare : Agent -> String
compare =
    .uuid >> Uuid.toString
