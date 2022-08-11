module ResourceType.ResourceType exposing (ResourceType, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Type exposing (Type)


type alias ResourceType =
    { what : Type
    , uuid : Uuid
    , parent : Maybe Uuid
    , group : Maybe Uuid
    }


encode : ResourceType -> Encode.Value
encode at =
    Encode.object <|
        [ ( "what", Type.encode at.what )
        , ( "uuid", Uuid.encode at.uuid )
        , ( "parent", Maybe.map Uuid.encode at.parent |> Maybe.withDefault Encode.null )
        ]
            ++ (Maybe.map (\g -> [ ( "group", Uuid.encode g ) ]) at.group |> Maybe.withDefault [])


decoder : Decode.Decoder ResourceType
decoder =
    Decode.map4 ResourceType
        (Decode.field "what" Type.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)
        (Decode.maybe (Decode.field "group" Uuid.decoder))


compare : ResourceType -> String
compare =
    toString


toString : ResourceType -> String
toString =
    .uuid >> Uuid.toString
