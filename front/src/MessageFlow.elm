module MessageFlow exposing (MessageFlow(..), decoder, encode, toString)

import Json.Decode as Decode
import Json.Encode as Encode


type
    MessageFlow
    -- Created and stored locally:
    = Requested
      -- Returned as Processed:
    | Processed
    | Error


toString : MessageFlow -> String
toString m =
    case m of
        Requested ->
            "Requested"

        Processed ->
            "Processed"

        Error ->
            "Error"


encode : MessageFlow -> Encode.Value
encode =
    toString >> Encode.string


decoder : Decode.Decoder MessageFlow
decoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "Requested" ->
                        Decode.succeed Requested

                    "Processed" ->
                        Decode.succeed Processed

                    "Error" ->
                        Decode.succeed Error

                    _ ->
                        Decode.fail "Unkown MessageFlow"
            )
