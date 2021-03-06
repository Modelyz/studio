module GroupType.GroupType exposing (GroupType, compare, decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe(..))
import Prng.Uuid as Uuid exposing (Uuid)


type alias GroupType =
    { uuid : Uuid
    , type_ : Maybe Uuid
    }


encode : GroupType -> Encode.Value
encode at =
    Encode.object
        [ ( "uuid", Uuid.encode at.uuid )
        , ( "type", Maybe.map Uuid.encode at.type_ |> Maybe.withDefault Encode.null )
        ]


decoder : Decode.Decoder GroupType
decoder =
    Decode.map2 GroupType
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "type" <| Decode.maybe Uuid.decoder)


compare : GroupType -> String
compare =
    toString


toString : GroupType -> String
toString =
    .uuid >> Uuid.toString
