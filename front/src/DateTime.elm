module DateTime exposing (decodeMonth, decodeWeekday, encodeMonth, encodeWeekday, monthToString, toStrMM, weekdayToString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time exposing (Month(..), Weekday(..))


toStrMM : Month -> String
toStrMM month =
    case month of
        Jan ->
            "01"

        Feb ->
            "02"

        Mar ->
            "03"

        Apr ->
            "04"

        May ->
            "05"

        Jun ->
            "06"

        Jul ->
            "07"

        Aug ->
            "08"

        Sep ->
            "09"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"


monthToString : Month -> String
monthToString month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


weekdayToString : Weekday -> String
weekdayToString weekday =
    case weekday of
        Mon ->
            "Mon"

        Tue ->
            "Tue"

        Wed ->
            "Wed"

        Thu ->
            "Thu"

        Fri ->
            "Fri"

        Sat ->
            "Sat"

        Sun ->
            "Sun"


decodeMonth : Decoder Month
decodeMonth =
    Decode.int
        |> Decode.andThen
            (\m ->
                case m of
                    1 ->
                        Decode.succeed Jan

                    2 ->
                        Decode.succeed Feb

                    3 ->
                        Decode.succeed Mar

                    4 ->
                        Decode.succeed Apr

                    5 ->
                        Decode.succeed May

                    6 ->
                        Decode.succeed Jun

                    7 ->
                        Decode.succeed Jul

                    8 ->
                        Decode.succeed Aug

                    9 ->
                        Decode.succeed Sep

                    10 ->
                        Decode.succeed Oct

                    11 ->
                        Decode.succeed Nov

                    12 ->
                        Decode.succeed Dec

                    _ ->
                        Decode.fail <| "Unkown month: " ++ String.fromInt m
            )


decodeWeekday : Decoder Weekday
decodeWeekday =
    Decode.int
        |> Decode.andThen
            (\m ->
                case m of
                    1 ->
                        Decode.succeed Mon

                    2 ->
                        Decode.succeed Tue

                    3 ->
                        Decode.succeed Wed

                    4 ->
                        Decode.succeed Thu

                    5 ->
                        Decode.succeed Fri

                    6 ->
                        Decode.succeed Sat

                    7 ->
                        Decode.succeed Sun

                    _ ->
                        Decode.fail <| "Unkown weekday: " ++ String.fromInt m
            )


encodeWeekday : Weekday -> Encode.Value
encodeWeekday =
    weekdayToString >> Encode.string


encodeMonth : Month -> Encode.Value
encodeMonth =
    monthToString >> Encode.string
