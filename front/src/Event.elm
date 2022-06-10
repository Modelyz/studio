port module Event exposing (Event(..), EventBase, EventPayload(..), base, compare, decodelist, decoder, encode, exceptCI, getTime, readEvents, storeEvents, storeEventsToSend)

import DictSet as Set
import EventFlow exposing (EventFlow, decoder)
import Json.Decode as Decode exposing (Decoder, andThen, decodeValue)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.Commitment as CM exposing (Commitment)
import REA.Entity as EN exposing (Entity)
import REA.EntityType as ENT exposing (EntityType)
import REA.Event as E
import REA.Group as G exposing (Group)
import REA.Ident as I exposing (EntityIdentifier, Fragment, Identifier, IdentifierType, encodeFragment, fragmentDecoder)
import REA.Process as P exposing (Process)
import REA.Restriction as Restriction exposing (Restriction)
import Time exposing (millisToPosix, posixToMillis)



-- read events from IDB


port readEvents : Encode.Value -> Cmd msg



-- store events to IDB then send to WS


port storeEvents : Encode.Value -> Cmd msg



-- only store to IDB


port storeEventsToSend : Encode.Value -> Cmd msg



-- application/user events --


type alias EventBase =
    { uuid : Uuid
    , when : Time.Posix
    , flow : EventFlow
    }


type Event
    = Event EventBase EventPayload


type EventPayload
    = ConnectionInitiated Connection
    | ProcessTypeChanged EntityType
    | ProcessTypeRemoved String
    | ProcessAdded Process
    | CommitmentAdded Commitment
    | EventAdded E.Event
    | Restricted Restriction
    | GroupAdded Group
    | GroupRemoved String
    | IdentifierTypeAdded IdentifierType
    | IdentifierTypeRemoved IdentifierType
    | Added Entity
    | Removed Entity
    | TypeAdded EntityType
    | TypeRemoved EntityType
    | IdentifierAdded EntityIdentifier


toString : EventPayload -> String
toString p =
    case p of
        ConnectionInitiated _ ->
            "ConnectionInitiated"

        ProcessTypeChanged _ ->
            "ProcessTypeChanged"

        ProcessTypeRemoved _ ->
            "ProcessTypeRemoved"

        ProcessAdded _ ->
            "ProcessAdded"

        CommitmentAdded _ ->
            "CommitmentAdded"

        TypeRemoved _ ->
            "TypeRemoved"

        EventAdded _ ->
            "EventAdded"

        Restricted _ ->
            "Restricted"

        GroupAdded _ ->
            "GroupAdded"

        GroupRemoved _ ->
            "GroupRemoved"

        IdentifierTypeAdded _ ->
            "IdentifierTypeAdded"

        IdentifierTypeRemoved _ ->
            "IdentifierTypeRemoved"

        TypeAdded _ ->
            "TypeAdded"

        Added _ ->
            "Added"

        Removed _ ->
            "Removed"

        IdentifierAdded _ ->
            "IdentifierAdded"


type alias Connection =
    -- TODO move in its module?
    { lastEventTime : Time.Posix, uuids : Set.DictSet String Uuid }


base : Event -> EventBase
base (Event b p) =
    b


compare : Event -> Int
compare =
    getTime >> posixToMillis


getTime : Event -> Time.Posix
getTime =
    base >> .when


exceptCI : List Event -> List Event
exceptCI es =
    List.filter
        (\(Event b p) ->
            case p of
                ConnectionInitiated _ ->
                    False

                _ ->
                    True
        )
        es



-- JSON encoding / decoding


encodeBase : EventBase -> Encode.Value
encodeBase b =
    Encode.object
        [ ( "uuid", Uuid.encode b.uuid )
        , ( "when", Encode.int <| posixToMillis b.when )
        , ( "flow", EventFlow.encode b.flow )
        ]


encode : Event -> Encode.Value
encode (Event b p) =
    Encode.object
        [ ( "what", Encode.string <| toString p )
        , ( "meta", encodeBase b )
        , case p of
            ProcessTypeChanged pt ->
                ( "load", ENT.encode pt )

            ProcessTypeRemoved pt ->
                ( "load", Encode.string pt )

            ProcessAdded pr ->
                ( "load", P.encode pr )

            CommitmentAdded cm ->
                ( "load", CM.encode cm )

            Restricted r ->
                ( "load", Restriction.encode r )

            TypeRemoved et ->
                ( "load", ENT.encode et )

            EventAdded e ->
                ( "load", E.encode e )

            ConnectionInitiated e ->
                ( "load"
                , Encode.object
                    [ ( "lastEventTime", Encode.int <| posixToMillis e.lastEventTime )
                    , ( "uuids", Encode.list Uuid.encode <| Set.toList e.uuids )
                    ]
                )

            GroupAdded g ->
                ( "load", G.encode g )

            GroupRemoved g ->
                ( "load", Encode.string g )

            IdentifierTypeAdded it ->
                ( "load", I.encodeIdentifierType it )

            IdentifierTypeRemoved it ->
                ( "load", I.encodeIdentifierType it )

            TypeAdded e ->
                ( "load", ENT.encode e )

            Added e ->
                ( "load", EN.encode e )

            Removed e ->
                ( "load", EN.encode e )

            IdentifierAdded eid ->
                ( "load", I.encodeEntityIdentifier eid )
        ]


decodelist : Decode.Value -> List Event
decodelist =
    Result.withDefault [] << decodeValue (Decode.list decoder)


toPosix : Int -> Decoder Time.Posix
toPosix t =
    Decode.succeed (millisToPosix t)


baseDecoder : Decoder EventBase
baseDecoder =
    Decode.map3 EventBase
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "when" Decode.int |> andThen toPosix)
        (Decode.field "flow" EventFlow.decoder)


decoder : Decoder Event
decoder =
    Decode.map2 Event
        (Decode.field "meta" baseDecoder)
        (Decode.field "what" Decode.string
            |> andThen
                (\t ->
                    case t of
                        "ProcessTypeChanged" ->
                            Decode.map ProcessTypeChanged
                                (Decode.field "load" ENT.decoder)

                        "ProcessTypeRemoved" ->
                            Decode.map ProcessTypeRemoved
                                (Decode.field "load" Decode.string)

                        "ProcessAdded" ->
                            Decode.map ProcessAdded
                                (Decode.field "load" P.decoder)

                        "CommitmentAdded" ->
                            Decode.map CommitmentAdded
                                (Decode.field "load" CM.decoder)

                        "Restricted" ->
                            Decode.map Restricted
                                (Decode.field "load" Restriction.decoder)

                        "TypeRemoved" ->
                            Decode.map TypeRemoved
                                (Decode.field "load" ENT.decoder)

                        "EventAdded" ->
                            Decode.map EventAdded
                                (Decode.field "load" E.decoder)

                        "ConnectionInitiated" ->
                            Decode.map ConnectionInitiated
                                (Decode.field "load"
                                    (Decode.map2 Connection
                                        (Decode.field "lastEventTime" Decode.int |> andThen toPosix)
                                        (Decode.field "uuids" (Decode.list Uuid.decoder) |> andThen (\xs -> Decode.succeed (Set.fromList Uuid.toString xs)))
                                    )
                                )

                        "GroupAdded" ->
                            Decode.map GroupAdded
                                (Decode.field "load" G.decoder)

                        "GroupRemoved" ->
                            Decode.map GroupRemoved
                                (Decode.field "load" Decode.string)

                        "IdentifierTypeAdded" ->
                            Decode.map IdentifierTypeAdded
                                (Decode.field "load" I.identifierTypeDecoder)

                        "IdentifierTypeRemoved" ->
                            Decode.map IdentifierTypeRemoved
                                (Decode.field "load" I.identifierTypeDecoder)

                        "TypeAdded" ->
                            Decode.map TypeAdded
                                (Decode.field "load" ENT.decoder)

                        "Added" ->
                            Decode.map Added
                                (Decode.field "load" EN.decoder)

                        "Removed" ->
                            Decode.map Removed
                                (Decode.field "load" EN.decoder)

                        "IdentifierAdded" ->
                            Decode.map IdentifierAdded
                                (Decode.field "load" I.entityIdentifierDecoder)

                        _ ->
                            Decode.fail <| "Unknown Event type: " ++ t
                )
        )
