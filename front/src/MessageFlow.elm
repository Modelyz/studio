module MessageFlow exposing (MessageFlow(..), decoder, encode)

import Json.Decode as Decode
import Json.Encode as Encode


type
    MessageFlow
    -- Created and stored locally:
    = Requested
      -- Received by the backend:
    | Received
      -- Returned as Processed:
    | Processed
    | Error String


encode : MessageFlow -> Encode.Value
encode f =
    case f of
        Requested ->
            Encode.object [ ( "type", Encode.string "Requested" ) ]

        Received ->
            Encode.object [ ( "type", Encode.string "Received" ) ]

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

                    "Received" ->
                        Decode.succeed Received

                    "Processed" ->
                        Decode.succeed Processed

                    "Error" ->
                        Decode.map Error (Decode.field "value" Decode.string)

                    _ ->
                        Decode.fail "Unkown MessageFlow"
            )
