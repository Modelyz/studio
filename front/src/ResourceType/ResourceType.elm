module ResourceType.ResourceType exposing (ResourceType, decoder, encode)

import Hierarchy.Type as HType
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Prng.Uuid as Uuid exposing (Uuid)


type alias ResourceType =
    { what : HType.Type
    , uuid : Uuid
    , parent : Maybe Uuid
    }


encode : ResourceType -> Encode.Value
encode at =
    Encode.object <|
        [ ( "what", HType.encode at.what )
        , ( "uuid", Uuid.encode at.uuid )
        , ( "parent", Maybe.map Uuid.encode at.parent |> Maybe.withDefault Encode.null )
        ]


decoder : Decode.Decoder ResourceType
decoder =
    Decode.map3 ResourceType
        (Decode.field "what" HType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)
