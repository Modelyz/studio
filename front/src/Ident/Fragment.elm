module Ident.Fragment exposing (Fragment(..), Name, Padding, Start, Step, all, decoder, encode, toDesc, toString, toValue)

import DateTime exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time exposing (Month(..), Posix, Weekday(..), millisToPosix, posixToMillis)


type alias Name =
    String


type Fragment
    = Free String
    | Fixed String
    | Sequence Padding Step Start (Maybe String)
    | Existing Name String
    | YYYY Int
    | YY Int
    | MMMM Month
    | MM Month
    | Weekday Weekday
    | DoM Int
    | Hour Int
    | Minute Int
    | Second Int
    | DateFrom Name Posix


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
    , Sequence 4 1 0 Nothing
    , Existing "" ""
    , YYYY 0
    , YY 0
    , MMMM Jan
    , MM Jan
    , Weekday Mon
    , DoM 0
    , Hour 0
    , Minute 0
    , Second 0
    , DateFrom "" (millisToPosix 0)
    ]


toString : Fragment -> String
toString f =
    case f of
        Free _ ->
            "Free"

        Fixed _ ->
            "Fixed"

        Sequence _ _ _ _ ->
            "Sequence"

        Existing _ _ ->
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

        DateFrom _ _ ->
            "Date from another field"


toValue : Fragment -> String
toValue f =
    case f of
        Free value ->
            value

        Fixed value ->
            value

        Sequence _ _ _ value ->
            Maybe.withDefault "(Not yet assigned)" value

        Existing _ value ->
            value

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

        DateFrom _ value ->
            String.fromInt <| posixToMillis value


toDesc : Fragment -> String
toDesc f =
    case f of
        Free _ ->
            "A free text that you will be able to enter to identify a new entity."

        Fixed _ ->
            "A fixed text that you define now and that will be always the same. For instance it can be a prefix, a suffix, a separator."

        Sequence _ _ _ _ ->
            "A sequence number used to identify an entity."

        Existing _ _ ->
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

        DateFrom from _ ->
            "Date coming from another field: " ++ from


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

        Sequence padding step start value ->
            Encode.object
                [ ( "type", Encode.string "Sequence" )
                , ( "padding", Encode.int padding )
                , ( "step", Encode.int step )
                , ( "start", Encode.int start )
                , ( "value", Maybe.map Encode.string value |> Maybe.withDefault Encode.null )
                ]

        Existing name value ->
            Encode.object
                [ ( "type", Encode.string "Existing" )
                , ( "name", Encode.string name )
                , ( "value", Encode.string value )
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

        DateFrom name value ->
            Encode.object
                [ ( "type", Encode.string "DateFrom" )
                , ( "name", Encode.string name )
                , ( "value", Encode.int (posixToMillis value) )
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
                        Decode.map4 Sequence
                            (Decode.field "padding" Decode.int)
                            (Decode.field "step" Decode.int)
                            (Decode.field "start" Decode.int)
                            (Decode.field "value" (Decode.maybe Decode.string))

                    "Existing" ->
                        Decode.map2 Existing (Decode.field "name" Decode.string) (Decode.field "value" Decode.string)

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
                        Decode.map2 DateFrom (Decode.field "name" Decode.string) (Decode.field "value" Decode.int |> Decode.andThen (millisToPosix >> Decode.succeed))

                    _ ->
                        Decode.fail <| "Unknown Sequence Fragment: " ++ t
            )
