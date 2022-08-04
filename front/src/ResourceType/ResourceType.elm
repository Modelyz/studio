module ResourceType.ResourceType exposing (ResourceType, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid as Uuid exposing (Uuid)


type alias ResourceType =
    { uuid : Uuid
    , type_ : Maybe Uuid
    , group : Maybe Uuid
    }


encode : ResourceType -> Encode.Value
encode at =
    Encode.object <|
        [ ( "uuid", Uuid.encode at.uuid )
        , ( "type", Maybe.map Uuid.encode at.type_ |> Maybe.withDefault Encode.null )
        ]
            ++ (Maybe.map (\g -> [ ( "group", Uuid.encode g ) ]) at.group |> Maybe.withDefault [])


decoder : Decode.Decoder ResourceType
decoder =
    Decode.map3 ResourceType
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" <| Decode.maybe Uuid.decoder)
        (Decode.maybe (Decode.field "group" Uuid.decoder))


compare : ResourceType -> String
compare =
    toString


toString : ResourceType -> String
toString =
    .uuid >> Uuid.toString
