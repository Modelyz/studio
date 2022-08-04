module Resource.Resource exposing (Resource, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)


type alias Resource =
    { uuid : Uuid
    , type_ : Uuid
    , group : Maybe Uuid
    }


compare : Resource -> String
compare =
    .uuid >> Uuid.toString


encode : Resource -> Encode.Value
encode r =
    Encode.object <|
        [ ( "uuid", Uuid.encode r.uuid )
        , ( "type", Uuid.encode r.type_ )
        ]
            ++ (Maybe.map (\g -> [ ( "group", Uuid.encode g ) ]) r.group |> Maybe.withDefault [])


decoder : Decoder Resource
decoder =
    Decode.map3 Resource
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" Uuid.decoder)
        (Decode.maybe (Decode.field "group" Uuid.decoder))
