module DateTime exposing (decodeMonth, decodeWeekday, encodeMonth, encodeWeekday, monthToLongString, monthToString, toStrMM, toString, weekdayToLongString, weekdayToString, zoneNameToString)

import Element exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time exposing (Month(..), Posix, Weekday(..), Zone)


toString : Zone -> Posix -> String
toString zone posix =
    (weekdayToLongString <| Time.toWeekday zone posix)
        ++ ", "
        ++ (String.fromInt <| Time.toDay zone posix)
        ++ " "
        ++ (monthToLongString <| Time.toMonth zone posix)
        ++ " "
        ++ (String.fromInt <| Time.toYear zone posix)


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


monthToLongString : Month -> String
monthToLongString month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


weekdayToLongString : Weekday -> String
weekdayToLongString weekday =
    case weekday of
        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"

        Sun ->
            "Sunday"


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
    Decode.string
        |> Decode.andThen
            (\m ->
                case m of
                    "Jan" ->
                        Decode.succeed Jan

                    "Feb" ->
                        Decode.succeed Feb

                    "Mar" ->
                        Decode.succeed Mar

                    "Apr" ->
                        Decode.succeed Apr

                    "May" ->
                        Decode.succeed May

                    "Jun" ->
                        Decode.succeed Jun

                    "Jul" ->
                        Decode.succeed Jul

                    "Aug" ->
                        Decode.succeed Aug

                    "Sep" ->
                        Decode.succeed Sep

                    "Oct" ->
                        Decode.succeed Oct

                    "Nov" ->
                        Decode.succeed Nov

                    "Dec" ->
                        Decode.succeed Dec

                    _ ->
                        Decode.fail <| "Unkown month: " ++ m
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


zoneNameToString : Time.ZoneName -> String
zoneNameToString zn =
    case zn of
        Time.Name name ->
            name

        Time.Offset offset ->
            String.fromInt offset
