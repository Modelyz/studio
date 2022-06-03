module REA.Ident exposing (..)

import DateTime exposing (..)
import DictSet as Set exposing (DictSet)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import REA.Entity as Entity exposing (Entity)
import REA.EntityType as ENT
import Time exposing (Month(..), Posix, Weekday(..), millisToPosix, posixToMillis)



-- Where do we get the date from
-- The IDntifier is the definition of an identifierType


type alias Name =
    String


type alias IdentifierType =
    -- This is the configuration of an identifier
    -- TODO rename to IdentifierType
    { entity : Entity
    , name : Name
    , fragments : List Fragment
    , applyTo : ENT.EntityTypes
    , unique : Bool
    , mandatory : Bool
    }


type alias Identifier =
    -- This is the value of an idenfitier
    { entity : Entity
    , name : Name
    , fragments : List Fragment
    }


type Fragment
    = Free String
    | Fixed String
    | Sequence Padding Step Int
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


compareIdentifierType : IdentifierType -> String
compareIdentifierType i =
    Entity.toString i.entity ++ " " ++ i.name


allFragments : List Fragment
allFragments =
    [ Free ""
    , Fixed ""
    , Sequence 4 1 0
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
fragmentToString f =
    case f of
        Free value ->
            "Free"

        Fixed value ->
            "Fixed"

        Sequence padding step value ->
            "Sequence"

        Existing name value ->
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


fragmentToName : Fragment -> Maybe String
fragmentToName f =
    case f of
        Free value ->
            Nothing

        Fixed value ->
            Nothing

        Sequence padding step value ->
            Nothing

        Existing name value ->
            Just name

        YYYY value ->
            Nothing

        YY value ->
            Nothing

        MMMM value ->
            Nothing

        MM value ->
            Nothing

        Weekday value ->
            Nothing

        DoM value ->
            Nothing

        Hour value ->
            Nothing

        Minute value ->
            Nothing

        Second value ->
            Nothing

        DateFrom from value ->
            Nothing


fragmentToValue : Fragment -> String
fragmentToValue f =
    case f of
        Free value ->
            value

        Fixed value ->
            value

        Sequence padding step value ->
            String.padLeft padding '0' <| String.fromInt value

        Existing name value ->
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


fragmentToDesc : Maybe Entity -> Fragment -> String
fragmentToDesc e f =
    let
        entity =
            Maybe.map Entity.toString e |> Maybe.withDefault "entity"
    in
    case f of
        Free _ ->
            "A free text that you will be able to enter to identify a new " ++ entity ++ "."

        Fixed _ ->
            "A fixed text that you must configure now and that will be always the same. For instance it can be a prefix or a suffix"

        Sequence _ _ _ ->
            "A sequence number used to identify a " ++ entity ++ "."

        Existing name _ ->
            "An existing identifierType for a " ++ entity ++ "."

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


encodeFragment : Fragment -> Encode.Value
encodeFragment =
    \f ->
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

            Sequence padding step value ->
                Encode.object
                    [ ( "type", Encode.string "Sequence" )
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


encodeIdentifierType : IdentifierType -> Encode.Value
encodeIdentifierType e =
    Encode.object
        [ ( "entity", Entity.encode e.entity )
        , ( "name", Encode.string e.name )
        , ( "fragments", Encode.list encodeFragment e.fragments )
        , ( "applyTo", ENT.encodeType e.applyTo )
        , ( "unique", Encode.bool e.unique )
        , ( "mandatory", Encode.bool e.mandatory )
        ]


encodeIdentifier : Identifier -> Encode.Value
encodeIdentifier e =
    Encode.object
        [ ( "entity", Entity.encode e.entity )
        , ( "name", Encode.string e.name )
        , ( "fragments", Encode.list encodeFragment e.fragments )
        ]


identifierTypeDecoder : Decoder IdentifierType
identifierTypeDecoder =
    Decode.map6 IdentifierType
        (Decode.field "entity" Entity.decoder)
        (Decode.field "name" Decode.string)
        (Decode.field "fragments" (Decode.list fragmentDecoder))
        (Decode.field "applyTo" ENT.typesDecoder)
        (Decode.field "unique" Decode.bool)
        (Decode.field "mandatory" Decode.bool)


identifierDecoder : Decoder Identifier
identifierDecoder =
    Decode.map3 Identifier
        (Decode.field "entity" Entity.decoder)
        (Decode.field "name" Decode.string)
        (Decode.field "fragments" (Decode.list fragmentDecoder))


fragmentDecoder : Decoder Fragment
fragmentDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "Free" ->
                        Decode.map Free (Decode.field "value" Decode.string)

                    "Fixed" ->
                        Decode.map Fixed (Decode.field "value" Decode.string)

                    "Sequence" ->
                        Decode.map3 Sequence
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


updateIdentifierType : Int -> Fragment -> IdentifierType -> IdentifierType
updateIdentifierType index fragment identifierType =
    let
        fragments =
            identifierType.fragments
                |> List.indexedMap Tuple.pair
                |> List.map
                    (\( i, f ) ->
                        if i == index then
                            fragment

                        else
                            f
                    )
    in
    { identifierType | fragments = fragments }


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


fromIdentifierType : IdentifierType -> Identifier
fromIdentifierType i =
    { entity = i.entity, name = i.name, fragments = i.fragments }
