module GroupType.GroupType exposing (GroupType, decoder, encode)

import Hierarchy.Type as HType
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Prng.Uuid as Uuid exposing (Uuid)


type alias GroupType =
    { what : HType.Type
    , uuid : Uuid
    , parent : Maybe Uuid
    }


encode : GroupType -> Encode.Value
encode gt =
    Encode.object
        [ ( "what", HType.encode gt.what )
        , ( "uuid", Uuid.encode gt.uuid )
        , ( "parent", Maybe.map Uuid.encode gt.parent |> Maybe.withDefault Encode.null )
        ]


decoder : Decode.Decoder GroupType
decoder =
    Decode.map3 GroupType
        (Decode.field "what" HType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)
