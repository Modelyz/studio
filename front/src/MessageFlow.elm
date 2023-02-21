module MessageFlow exposing (MessageFlow(..), decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode


type
    MessageFlow
    -- Created and stored locally:
    = Requested
      -- Sent to the backend:
    | Sent
      -- Returned as Processed:
    | Processed


toString : MessageFlow -> String
toString f =
    case f of
        Requested ->
            "Requested"

        Sent ->
            "Sent"

        Processed ->
            "Processed"


encode : MessageFlow -> Encode.Value
encode f =
    Encode.string
        (toString f)


decoder : Decode.Decoder MessageFlow
decoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "Requested" ->
                        Decode.succeed Requested

                    "Sent" ->
                        Decode.succeed Sent

                    "Processed" ->
                        Decode.succeed Processed

                    _ ->
                        Decode.fail "Unkown MessageFlow"
            )
