module REA.Ident exposing (..)

import DateTime exposing (..)
import DictSet as Set exposing (DictSet)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Prng.Uuid as Uuid
import REA.Entity as EN exposing (Entity, toUuid)
import REA.EntityType as ENT exposing (toName)
import Time exposing (Month(..), Posix, Weekday(..), millisToPosix, posixToMillis)


type alias Name =
    String


type
    Scope
    -- This is the scope of an identifier
    -- We can identify a particular entity:
    = OneEntity EN.Entity
      -- or a certain entity type:
    | OneEntityType ENT.EntityType
      -- or all the entities of a certain type:
    | AllEntityTypes ENT.EntityType
      -- or all the entity types of a certain type:
    | AllEntities ENT.EntityType



-- TODO: also identify all entities or of a group ?


type alias IdentifierType =
    -- This is the configuration of an identifier
    { name : Name
    , fragments : List Fragment
    , applyTo : DictSet String Scope
    , unique : Bool
    , mandatory : Bool
    }


type alias Identifier =
    -- This is the value of an idenfiier (maybe before the entity even exists)
    { name : Name
    , fragments : List Fragment
    }


type
    Identifiable
    -- What is identified
    = Entity EN.Entity
    | EntityType ENT.EntityType


type alias EntityIdentifier =
    -- the link between an identifiable and its identifier
    { identifiable : Identifiable
    , identifier : Identifier
    }


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
    | OtherIdentifier Name


type alias Padding =
    Int


type alias Start =
    Int


type alias Step =
    Int


getIdentifier : String -> DictSet String Identifier -> Maybe Identifier
getIdentifier name =
    Set.filter (\i -> i.name == name) >> Set.toList >> List.head


toDesc : Scope -> String
toDesc id =
    case id of
        OneEntity e ->
            "entity " ++ Uuid.toString (toUuid e)

        OneEntityType et ->
            toName et

        AllEntities et ->
            "entities of type " ++ toName et

        AllEntityTypes et ->
            "types whose parent type is " ++ toName et


toString : Scope -> String
toString id =
    case id of
        OneEntity e ->
            "OneEntity"

        OneEntityType et ->
            "OneEntityType"

        AllEntities et ->
            "AllEntities"

        AllEntityTypes et ->
            "AllEntityTypes"


compareEntityIdentifier : EntityIdentifier -> String
compareEntityIdentifier ei =
    compareIdentifiable ei.identifiable ++ ei.identifier.name


compareIdentifiable : Identifiable -> String
compareIdentifiable i =
    case i of
        Entity e ->
            "Entity " ++ EN.compare e

        EntityType et ->
            "EntityType " ++ ENT.compare et


compareIdentifier : Identifier -> String
compareIdentifier =
    .name


compareIdentifierType : IdentifierType -> String
compareIdentifierType =
    .name


allFragments : List Fragment
allFragments =
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
    , OtherIdentifier ""
    ]


fragmentToString : Fragment -> String
fragmentToString f =
    case f of
        Free value ->
            "Free"

        Fixed value ->
            "Fixed"

        Sequence padding step start value ->
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
            "Date from another field"

        OtherIdentifier name ->
            "Value from another identifier"


fragmentToName : Fragment -> Maybe String
fragmentToName f =
    case f of
        Free value ->
            Nothing

        Fixed value ->
            Nothing

        Sequence padding step start value ->
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

        OtherIdentifier name ->
            Just name


fragmentToValue : Fragment -> String
fragmentToValue f =
    case f of
        Free value ->
            value

        Fixed value ->
            value

        Sequence padding step start value ->
            Maybe.withDefault "(Not yet assigned)" value

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

        OtherIdentifier name ->
            "??"


fragmentToDesc : Fragment -> String
fragmentToDesc f =
    case f of
        Free _ ->
            "A free text that you will be able to enter to identify a new entity."

        Fixed _ ->
            "A fixed text that you define now and that will be always the same. For instance it can be a prefix, a suffix, a separator."

        Sequence _ _ _ _ ->
            "A sequence number used to identify an entity."

        Existing name _ ->
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

        OtherIdentifier name ->
            "Value from the identifier: " ++ name


encodeFragment : Fragment -> Encode.Value
encodeFragment f =
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

        OtherIdentifier name ->
            Encode.object
                [ ( "type", Encode.string "OtherIdentifier" )
                , ( "name", Encode.string name )
                ]


encodeIdentifierType : IdentifierType -> Encode.Value
encodeIdentifierType e =
    Encode.object
        [ ( "name", Encode.string e.name )
        , ( "fragments", Encode.list encodeFragment e.fragments )
        , ( "applyTo", Encode.list encodeScope <| Set.toList e.applyTo )
        , ( "unique", Encode.bool e.unique )
        , ( "mandatory", Encode.bool e.mandatory )
        ]


encodeIdentifiable : Identifiable -> Encode.Value
encodeIdentifiable i =
    case i of
        Entity e ->
            EN.encode e

        EntityType et ->
            ENT.encode et


encodeScope : Scope -> Encode.Value
encodeScope e =
    case e of
        OneEntity entity ->
            Encode.object
                [ ( "for", Encode.string "OneEntity" )
                , ( "entity", EN.encode entity )
                ]

        OneEntityType entityType ->
            Encode.object
                [ ( "for", Encode.string "OneEntityType" )
                , ( "type", ENT.encode entityType )
                ]

        AllEntities entityType ->
            Encode.object
                [ ( "for", Encode.string "AllEntities" )
                , ( "type", ENT.encode entityType )
                ]

        AllEntityTypes entityType ->
            Encode.object
                [ ( "for", Encode.string "AllEntityTypes" )
                , ( "type", ENT.encode entityType )
                ]


encodeEntityIdentifier : EntityIdentifier -> Encode.Value
encodeEntityIdentifier ei =
    Encode.object
        [ ( "identifiable", encodeIdentifiable ei.identifiable )
        , ( "identifier", encodeIdentifier ei.identifier )
        ]


entityIdentifierDecoder : Decoder EntityIdentifier
entityIdentifierDecoder =
    Decode.map2 EntityIdentifier
        (Decode.field "identifiable" identifiableDecoder)
        (Decode.field "identifier" identifierDecoder)


scopeDecoder : Decoder Scope
scopeDecoder =
    Decode.field "for" Decode.string
        |> Decode.andThen
            (\for ->
                case for of
                    "OneEntity" ->
                        Decode.field "type" (Decode.map OneEntity EN.decoder)

                    "OneEntityType" ->
                        Decode.field "type" (Decode.map OneEntityType ENT.decoder)

                    "AllEntities" ->
                        Decode.field "type" (Decode.map AllEntities ENT.decoder)

                    "AllEntityTypes" ->
                        Decode.field "type" (Decode.map AllEntityTypes ENT.decoder)

                    _ ->
                        Decode.fail "Cannot decode the scope of this identifier type"
            )


encodeIdentifier : Identifier -> Encode.Value
encodeIdentifier e =
    Encode.object
        [ ( "name", Encode.string e.name )
        , ( "fragments", Encode.list encodeFragment e.fragments )
        ]


identifierTypeDecoder : Decoder IdentifierType
identifierTypeDecoder =
    Decode.map5 IdentifierType
        (Decode.field "name" Decode.string)
        (Decode.field "fragments" (Decode.list fragmentDecoder))
        (Decode.field "applyTo" (Decode.list scopeDecoder |> Decode.andThen (Set.fromList compare >> Decode.succeed)))
        (Decode.field "unique" Decode.bool)
        (Decode.field "mandatory" Decode.bool)


identifiableDecoder : Decoder Identifiable
identifiableDecoder =
    -- some kind of intrusion into EN and ENT decoders
    Decode.field "what" Decode.string
        |> Decode.andThen
            (\w ->
                if (String.slice 0 4 <| String.reverse w) == "Type" then
                    Decode.map EntityType ENT.decoder

                else
                    Decode.map Entity EN.decoder
            )


identifierDecoder : Decoder Identifier
identifierDecoder =
    Decode.map2 Identifier
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

                    "OtherIdentifier" ->
                        Decode.map OtherIdentifier (Decode.field "name" Decode.string)

                    _ ->
                        Decode.fail <| "Unknown Sequence Fragment: " ++ t
            )


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
fromIdentifierType it =
    { name = it.name, fragments = it.fragments }


compare : Scope -> String
compare i =
    toString i
        ++ " "
        ++ (case i of
                OneEntity e ->
                    EN.compare e

                OneEntityType et ->
                    ENT.compare et

                AllEntities et ->
                    ENT.compare et

                AllEntityTypes et ->
                    ENT.compare et
           )
