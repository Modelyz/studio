module Service exposing (Service(..), decoder, encode, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Service
    = None
    | Front
    | Studio
    | Store
    | Ident


toString : Service -> String
toString s =
    case s of
        None ->
            "None"

        Front ->
            "Front"

        Studio ->
            "Studio"

        Store ->
            "Store"

        Ident ->
            "Ident"


encode : Service -> Encode.Value
encode =
    toString >> Encode.string


decoder : Decoder Service
decoder =
    Decode.string
        |> Decode.andThen
            (\o ->
                case o of
                    "None" ->
                        Decode.succeed None

                    "Front" ->
                        Decode.succeed Front

                    "Studio" ->
                        Decode.succeed Studio

                    "Store" ->
                        Decode.succeed Store

                    "Ident" ->
                        Decode.succeed Ident

                    _ ->
                        Decode.fail "Unknown Service"
            )
