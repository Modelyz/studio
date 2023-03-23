module ProcessType.ProcessType exposing (ProcessType, compare, decoder, encode)

import Dict exposing (Dict)
import Hierarchy.Type as HType
import Json.Decode as Decode
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Prng.Uuid as Uuid exposing (Uuid)


type alias ProcessType =
    { what : HType.Type
    , uuid : Uuid
    , parent : Maybe Uuid
    , eventTypes : Dict String Uuid
    }


encode : ProcessType -> Encode.Value
encode pt =
    Encode.object
        [ ( "what", HType.encode pt.what )
        , ( "uuid", Uuid.encode pt.uuid )
        , ( "parent", Maybe.map Uuid.encode pt.parent |> Maybe.withDefault Encode.null )
        , ( "eventTypes", Dict.values pt.eventTypes |> Encode.list Uuid.encode )
        ]


decoder : Decode.Decoder ProcessType
decoder =
    Decode.map4 ProcessType
        (Decode.field "what" HType.decoder)
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "parent" <| Decode.maybe Uuid.decoder)
        (Decode.field "eventTypes" (Decode.list Uuid.decoder)
            |> Decode.andThen (List.map (\u -> ( Uuid.toString u, u )) >> Dict.fromList >> Decode.succeed)
        )


compare : ProcessType -> String
compare =
    Uuid.toString << .uuid
