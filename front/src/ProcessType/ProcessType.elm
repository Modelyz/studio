module ProcessType.ProcessType exposing (ProcessType, decoder, encode)

import Hierarchy.Type as HType
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Prng.Uuid as Uuid exposing (Uuid)


type alias ProcessType =
    { what : HType.Type
    , uuid : Uuid
    , parent : Maybe Uuid
    }


encode : ProcessType -> Encode.Value
encode pt =
    Encode.object
        [ ( "what", HType.encode pt.what )
        , ( "uuid", Uuid.encode pt.uuid )
        , ( "parent", Maybe.map Uuid.encode pt.parent |> Maybe.withDefault Encode.null )
        ]


decoder : Decode.Decoder ProcessType
decoder =
    Decode.map3 ProcessType
        (Decode.field "what" HType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)
