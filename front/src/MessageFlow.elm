module MessageFlow exposing (MessageFlow(..), decoder, encode, toString)

import Json.Decode as Decode
import Json.Encode as Encode


type
    MessageFlow
    -- Created and stored locally:
    = Requested
      -- Returned as Processed:
    | Processed
    | Error String


toString : MessageFlow -> String
toString m =
    case m of
        Requested ->
            "Requested"

        Processed ->
            "Processed"

        Error err ->
            "Error: " ++ err


encode : MessageFlow -> Encode.Value
encode f =
    case f of
        Requested ->
            Encode.object [ ( "type", Encode.string "Requested" ) ]

        Processed ->
            Encode.object [ ( "type", Encode.string "Processed" ) ]

        Error err ->
            Encode.object [ ( "type", Encode.string "Error" ), ( "value", Encode.string err ) ]


decoder : Decode.Decoder MessageFlow
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "Requested" ->
                        Decode.succeed Requested

                    "Processed" ->
                        Decode.succeed Processed

                    "Error" ->
                        Decode.map Error (Decode.field "value" Decode.string)

                    _ ->
                        Decode.fail "Unkown MessageFlow"
            )
