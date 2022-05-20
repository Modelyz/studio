module REA.Ident exposing (..)

import DateTime exposing (..)
import DictSet as Set exposing (DictSet)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import REA.Entity as Entity exposing (Entity)
import Time exposing (Month(..), Posix, Weekday(..), millisToPosix, posixToMillis)



-- Where do we get the date from
-- The IDntifier is the definition of an identification


type alias Name =
    String


type alias Identifier =
    -- This is the configuration of an identifier
    { name : Name
    , entity : Entity
    , unique : Bool
    , mandatory : Bool
    , fragments : List Fragment
    }


type Fragment
    = Free Name String
    | Fixed String
    | Sequence Name Padding Step Int
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


type alias Step =
    Int


compareIdentifier : Identifier -> String
compareIdentifier i =
    Entity.toString i.entity ++ " " ++ i.name


allFragments : List Fragment
allFragments =
    [ Free "" ""
    , Fixed ""
    , Sequence "" 4 1 0
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


fragmentToString : Fragment -> String
fragmentToString p =
    case p of
        Free name value ->
            "Free"

        Fixed value ->
            "Fixed"

        Sequence name padding step value ->
            "Sequence"

        Existing str value ->
            "Existing"

        YYYY value ->
            "YYYY"

        YY value ->
            "YY"

        MMMM value ->
            "MMMM"

        MM value ->
            "MM"

        Weekday value ->
            "Weekday"

        DoM value ->
            "DoM"

        Hour value ->
            "hh"

        Minute value ->
            "mm"

        Second value ->
            "ss"

        DateFrom from value ->
            "Date from: " ++ from


fragmentToValue : Fragment -> String
fragmentToValue p =
    case p of
        Free name value ->
            value

        Fixed value ->
            value

        Sequence name padding step value ->
            String.padLeft padding '0' <| String.fromInt value

        Existing str value ->
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

        DateFrom from value ->
            String.fromInt <| posixToMillis value


fragmentToDesc : Fragment -> String
fragmentToDesc p =
    case p of
        Free _ _ ->
            "A free text that you will be able to enter when adding a new TODO. For instance a firstname of an Agent"

        Fixed _ ->
            "A fixed text that you must configure now and that will be always the same. For instance it can be a prefix or a suffix"

        Sequence _ _ _ _ ->
            "A sequence number"

        Existing str _ ->
            "An existing identification"

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
            "Date coming from another entity: " ++ from


encodeFragment : Fragment -> Encode.Value
encodeFragment =
    \p ->
        case p of
            Free name value ->
                Encode.object
                    [ ( "type", Encode.string "Free" )
                    , ( "name", Encode.string name )
                    , ( "value", Encode.string value )
                    ]

            Fixed value ->
                Encode.object
                    [ ( "type", Encode.string "Fixed" )
                    , ( "value", Encode.string value )
                    ]

            Sequence name padding step value ->
                Encode.object
                    [ ( "type", Encode.string "Sequence" )
                    , ( "name", Encode.string name )
                    , ( "padding", Encode.int padding )
                    , ( "step", Encode.int step )
                    , ( "value", Encode.int value )
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


fragmentDecoder : Decoder Fragment
fragmentDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "Free" ->
                        Decode.map2 Free (Decode.field "name" Decode.string) (Decode.field "value" Decode.string)

                    "Fixed" ->
                        Decode.map Fixed (Decode.field "value" Decode.string)

                    "Sequence" ->
                        Decode.map4 Sequence
                            (Decode.field "name" Decode.string)
                            (Decode.field "padding" Decode.int)
                            (Decode.field "step" Decode.int)
                            (Decode.field "value" Decode.int)

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


selectFrom : DictSet String Identifier -> Entity -> Name -> Maybe Identifier
selectFrom xs entity name =
    -- select an identifier given its entity and name (should be unique, given the compare function)
    Set.filter (\i -> i.entity == entity && i.name == name) xs |> Set.toList |> List.head


identifierValue : Identifier -> String
identifierValue i =
    i.fragments |> List.map fragmentToValue |> String.concat


updateIdentifier : Int -> Fragment -> Identifier -> Identifier
updateIdentifier index fragment identifier =
    let
        fragments =
            identifier.fragments
                |> List.indexedMap Tuple.pair
                |> List.map
                    (\( i, f ) ->
                        if i == index then
                            fragment

                        else
                            f
                    )
    in
    { identifier | fragments = fragments }
