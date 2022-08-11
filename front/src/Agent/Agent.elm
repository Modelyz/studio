module Agent.Agent exposing (Agent, compare, decoder, encode)

import Item.Item as Item exposing (Item)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type alias Agent =
    { what : Type
    , uuid : Uuid
    , type_ : Uuid
    , group : Maybe Uuid
    }


encode : Agent -> Encode.Value
encode a =
    Encode.object <|
        [ ( "what", Type.encode a.what )
        , ( "uuid", Uuid.encode a.uuid )
        , ( "type", Uuid.encode a.type_ )
        ]
            ++ (Maybe.map (\g -> [ ( "group", Uuid.encode g ) ]) a.group |> Maybe.withDefault [])


decoder : Decoder Agent
decoder =
    Decode.map4 Agent
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.maybe (Decode.field "group" Uuid.decoder))


compare : Agent -> String
compare =
    .uuid >> Uuid.toString
