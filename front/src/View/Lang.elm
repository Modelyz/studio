module View.Lang exposing (Lang, compare, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Lang
    = EN_us
    | FR_fr


toString : Lang -> String
toString lang =
    case lang of
        EN_us ->
            "EN_us"

        FR_fr ->
            "FR_fr"


compare : Lang -> String
compare =
    toString


encode : Lang -> Encode.Value
encode lang =
    Encode.string (toString lang)


decoder : Decoder Lang
decoder =
    Decode.string
        |> Decode.andThen
            (\lang ->
                case lang of
                    "EN_us" ->
                        Decode.succeed EN_us

                    "FR_fr" ->
                        Decode.succeed FR_fr

                    _ ->
                        Decode.fail <| "Unsupported lang: " ++ lang
            )
