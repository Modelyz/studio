module EventFlow exposing (EventFlow(..), decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode


type EventFlow
    = Requested
    | Processed


toString : EventFlow -> String
toString f =
    case f of
        Requested ->
            "Requested"

        Processed ->
            "Processed"


encode : EventFlow -> Encode.Value
encode f =
    Encode.string
        (toString f)


decoder : Decode.Decoder EventFlow
decoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "Requested" ->
                        Decode.succeed Requested

                    "Processed" ->
                        Decode.succeed Processed

                    _ ->
                        Decode.fail "Unkown EventFlow"
            )
