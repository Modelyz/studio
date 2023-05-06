module Ident.Fragment exposing (Fragment(..), Name, Padding, Start, Step, all, decoder, encode, toDesc, toString, toValue)

import DateTime exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import String exposing (padLeft)
import Time exposing (Month(..), Posix, Weekday(..), millisToPosix, posixToMillis)


type alias Name =
    String


type Fragment
    = Free String
    | Fixed String
    | Sequence { name : Name, padding : Padding, step : Step, start : Start, val : Maybe Int }
    | Existing { name : Name, value : String }
    | YYYY Int
    | YY Int
    | MMMM Month
    | MM Month
    | Weekday Weekday
    | DoM Int
    | Hour Int
    | Minute Int
    | Second Int
    | DateFrom { field : Name, when : Posix }


type alias Padding =
    Int


type alias Start =
    Int


type alias Step =
    Int


all : List Fragment
all =
    [ Free ""
    , Fixed ""
    , Sequence { name = "", padding = 4, step = 1, start = 0, val = Nothing }
    , Existing { name = "", value = "" }
    , YYYY 0
    , YY 0
    , MMMM Jan
    , MM Jan
    , Weekday Mon
    , DoM 0
    , Hour 0
    , Minute 0
    , Second 0
    , DateFrom { field = "", when = millisToPosix 0 }
    ]


toString : Fragment -> String
toString f =
    case f of
        Free _ ->
            "Free"

        Fixed _ ->
            "Fixed"

        Sequence _ ->
            "Sequence"

        Existing _ ->
            "Existing"

        YYYY _ ->
            "YYYY"

        YY _ ->
            "YY"

        MMMM _ ->
            "MMMM"

        MM _ ->
            "MM"

        Weekday _ ->
            "Weekday"

        DoM _ ->
            "DoM"

        Hour _ ->
            "hh"

        Minute _ ->
            "mm"

        Second _ ->
            "ss"

        DateFrom _ ->
            "Date from another field"


toValue : Fragment -> String
toValue f =
    case f of
        Free value ->
            value

        Fixed value ->
            value

        Sequence s ->
            padLeft s.padding '0' <| Maybe.withDefault "?" <| Maybe.map String.fromInt s.val

        Existing e ->
            e.value

        YYYY value ->
            String.fromInt value

        YY value ->
            String.fromInt value

        MMMM value ->
            monthToString value

        MM value ->
            toStrMM value

        Weekday value ->
            weekdayToString value

        DoM value ->
            String.fromInt value

        Hour value ->
            String.fromInt value

        Minute value ->
            String.fromInt value

        Second value ->
            String.fromInt value

        DateFrom d ->
            String.fromInt <| posixToMillis d.when


toDesc : Fragment -> String
toDesc f =
    case f of
        Free _ ->
            "A free text that you will be able to enter to identify a new entity."

        Fixed _ ->
            "A fixed text that you define now and that will be always the same. For instance it can be a prefix, a suffix, a separator."

        Sequence _ ->
            "A sequence number used to identify an entity. You can configure the name of the sequence, the number of figures, the increment and the starting number"

        Existing _ ->
            "An existing identifierType for an entity."

        YYYY _ ->
            "The year on 4 digits"

        YY _ ->
            "The year on 2 digits"

        MMMM _ ->
            "The month in long form. Ex: August"

        MM _ ->
            "The month on 2 digits."

        Weekday _ ->
            "Day of week in long form (Ex: Saturday)"

        DoM _ ->
            "Day of month on 2 digits"

        Hour _ ->
            "Hour on 2 digits"

        Minute _ ->
            "Minute on 2 digits"

        Second _ ->
            "Second on 2 digits"

        DateFrom d ->
            "Date coming from another field: " ++ d.field


encode : Fragment -> Encode.Value
encode f =
    case f of
        Free value ->
            Encode.object
                [ ( "type", Encode.string "Free" )
                , ( "value", Encode.string value )
                ]

        Fixed value ->
            Encode.object
                [ ( "type", Encode.string "Fixed" )
                , ( "value", Encode.string value )
                ]

        Sequence s ->
            Encode.object
                [ ( "type", Encode.string "Sequence" )
                , ( "name", Encode.string s.name )
                , ( "padding", Encode.int s.padding )
                , ( "step", Encode.int s.step )
                , ( "start", Encode.int s.start )
                , ( "val", Maybe.map Encode.int s.val |> Maybe.withDefault Encode.null )
                ]

        Existing e ->
            Encode.object
                [ ( "type", Encode.string "Existing" )
                , ( "name", Encode.string e.name )
                , ( "value", Encode.string e.value )
                ]

        YYYY value ->
            Encode.object
                [ ( "type", Encode.string "YYYY" )
                , ( "value", Encode.int value )
                ]

        YY value ->
            Encode.object
                [ ( "type", Encode.string "YY" )
                , ( "value", Encode.int value )
                ]

        MMMM value ->
            Encode.object
                [ ( "type", Encode.string "MMMM" )
                , ( "value", encodeMonth value )
                ]

        MM value ->
            Encode.object
                [ ( "type", Encode.string "MM" )
                , ( "value", encodeMonth value )
                ]

        Weekday value ->
            Encode.object
                [ ( "type", Encode.string "Weekday" )
                , ( "value", encodeWeekday value )
                ]

        DoM value ->
            Encode.object
                [ ( "type", Encode.string "DoM" )
                , ( "value", Encode.int value )
                ]

        Hour value ->
            Encode.object
                [ ( "type", Encode.string "Hour" )
                , ( "value", Encode.int value )
                ]

        Minute value ->
            Encode.object
                [ ( "type", Encode.string "Minute" )
                , ( "value", Encode.int value )
                ]

        Second value ->
            Encode.object
                [ ( "type", Encode.string "Second" )
                , ( "value", Encode.int value )
                ]

        DateFrom d ->
            Encode.object
                [ ( "type", Encode.string "DateFrom" )
                , ( "field", Encode.string d.field )
                , ( "when", Encode.int (posixToMillis d.when) )
                ]


decoder : Decoder Fragment
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "Free" ->
                        Decode.map Free (Decode.field "value" Decode.string)

                    "Fixed" ->
                        Decode.map Fixed (Decode.field "value" Decode.string)

                    "Sequence" ->
                        Decode.map5
                            (\name padding step start value ->
                                Sequence
                                    { name = name
                                    , padding = padding
                                    , step = step
                                    , start = start
                                    , val = value
                                    }
                            )
                            (Decode.field "name" Decode.string)
                            (Decode.field "padding" Decode.int)
                            (Decode.field "step" Decode.int)
                            (Decode.field "start" Decode.int)
                            (Decode.field "val" (Decode.maybe Decode.int))

                    "Existing" ->
                        Decode.map2 (\name value -> Existing { name = name, value = value })
                            (Decode.field "name" Decode.string)
                            (Decode.field "value" Decode.string)

                    "YYYY" ->
                        Decode.map YYYY (Decode.field "value" Decode.int)

                    "YY" ->
                        Decode.map YY (Decode.field "value" Decode.int)

                    "MMMM" ->
                        Decode.map MMMM (Decode.field "value" decodeMonth)

                    "MM" ->
                        Decode.map MM (Decode.field "value" decodeMonth)

                    "Weekday" ->
                        Decode.map Weekday (Decode.field "value" decodeWeekday)

                    "DoM" ->
                        Decode.map DoM (Decode.field "value" Decode.int)

                    "Hour" ->
                        Decode.map Hour (Decode.field "value" Decode.int)

                    "Minute" ->
                        Decode.map Minute (Decode.field "value" Decode.int)

                    "Second" ->
                        Decode.map Second (Decode.field "value" Decode.int)

                    "DateFrom" ->
                        Decode.map2 (\field when -> DateFrom { field = field, when = when })
                            (Decode.field "field" Decode.string)
                            (Decode.field "when" Decode.int |> Decode.andThen (millisToPosix >> Decode.succeed))

                    _ ->
                        Decode.fail <| "Unknown Sequence Fragment: " ++ t
            )
