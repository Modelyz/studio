module Resource.Resource exposing (Resource, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type alias Resource =
    { what : Type
    , uuid : Uuid
    , type_ : Uuid
    , group : Maybe Uuid
    }


compare : Resource -> String
compare =
    .uuid >> Uuid.toString


encode : Resource -> Encode.Value
encode r =
    Encode.object <|
        [ ( "what", Type.encode r.what )
        , ( "uuid", Uuid.encode r.uuid )
        , ( "type", Uuid.encode r.type_ )
        ]
            ++ (Maybe.map (\g -> [ ( "group", Uuid.encode g ) ]) r.group |> Maybe.withDefault [])


decoder : Decoder Resource
decoder =
    Decode.map4 Resource
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.maybe (Decode.field "group" Uuid.decoder))
