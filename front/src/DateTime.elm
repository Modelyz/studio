module DateTime exposing (decodeMonth, decodeWeekday, encodeMonth, encodeWeekday, monthToLongString, monthToString, toStrMM, toString, weekdayToLongString, weekdayToString, zoneNameToString)

import Calendar
import Date exposing (Date, fromPosix)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task
import Time exposing (Month(..), Posix, Weekday(..), Zone, millisToPosix)
import View.Style exposing (color, isMobile)


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


zoneNameToString : Time.ZoneName -> String
zoneNameToString zn =
    case zn of
        Time.Name name ->
            name

        Time.Offset offset ->
            String.fromInt offset
