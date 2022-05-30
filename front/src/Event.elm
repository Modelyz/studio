port module Event exposing (Event(..), EventBase, EventPayload(..), base, compare, decodelist, decoder, encode, exceptCI, getTime, readEvents, storeEvents, storeEventsToSend)

import DictSet as Set
import EventFlow exposing (EventFlow, decoder)
import IOStatus exposing (IOStatus(..))
import Json.Decode as Decode exposing (Decoder, andThen, decodeValue)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.Agent as A exposing (Agent)
import REA.AgentType as AT exposing (AgentType)
import REA.Commitment as CM exposing (Commitment)
import REA.CommitmentType as CT exposing (CommitmentType)
import REA.Entity exposing (Entity)
import REA.EntityType as ENT
import REA.Event as E
import REA.EventType as ET exposing (EventType)
import REA.Group as G exposing (Group)
import REA.Ident as I exposing (Fragment, Identification, Identifier, encodeFragment, fragmentDecoder)
import REA.Process as P exposing (Process)
import REA.ProcessType as PT exposing (ProcessType)
import REA.ProcessTypeCommitmentType as PTCT
import REA.ProcessTypeEventType as PTET
import Result exposing (Result(..))
import Time exposing (millisToPosix, posixToMillis)
import Websocket exposing (WSStatus(..))



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
    | ProcessTypeChanged ProcessType
    | ProcessTypeRemoved String
    | ProcessAdded Process
    | CommitmentTypeAdded CommitmentType
    | CommitmentTypeRemoved String
    | CommitmentAdded Commitment
    | EventTypeAdded EventType
    | EventTypeRemoved String
    | EventAdded E.Event
    | LinkedEventTypeToProcessType { etype : String, ptype : String } -- TODO single Link event?
    | LinkedCommitmentTypeToProcessType { ctype : String, ptype : String }
    | GroupAdded Group
    | GroupRemoved String
    | IdentificationAdded Identification
    | IdentificationRemoved String
    | AgentTypeAdded AgentType
    | AgentTypeRemoved String
    | AgentAdded Agent
    | AgentRemoved Uuid
    | IdentifierAdded Identifier


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

        CommitmentTypeAdded _ ->
            "CommitmentTypeAdded"

        CommitmentTypeRemoved _ ->
            "CommitmentTypeRemoved"

        CommitmentAdded _ ->
            "CommitmentAdded"

        EventTypeAdded _ ->
            "EventTypeAdded"

        EventTypeRemoved _ ->
            "EventTypeRemoved"

        EventAdded _ ->
            "EventAdded"

        LinkedEventTypeToProcessType _ ->
            "LinkedEventTypeToProcessType"

        LinkedCommitmentTypeToProcessType _ ->
            "LinkedCommitmentTypeToProcessType"

        GroupAdded _ ->
            "GroupAdded"

        GroupRemoved _ ->
            "GroupRemoved"

        IdentificationAdded _ ->
            "IdentificationAdded"

        IdentificationRemoved _ ->
            "IdentificationRemoved"

        AgentTypeAdded _ ->
            "AgentTypeAdded"

        AgentTypeRemoved _ ->
            "AgentTypeRemoved"

        AgentAdded _ ->
            "AgentAdded"

        AgentRemoved _ ->
            "AgentRemoved"

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


encodeBase : String -> EventBase -> Encode.Value
encodeBase t b =
    Encode.object
        [ ( "what", Encode.string t )
        , ( "uuid", Uuid.encode b.uuid )
        , ( "when", Encode.int <| posixToMillis b.when )
        , ( "flow", EventFlow.encode b.flow )
        ]


encode : Event -> Encode.Value
encode (Event b p) =
    Encode.object
        [ ( "what", Encode.string <| toString p )
        , ( "meta", encodeBase "ProcessTypeChanged" b )
        , case p of
            ProcessTypeChanged e ->
                ( "load", PT.encode e )

            ProcessTypeRemoved n ->
                ( "load", Encode.string n )

            ProcessAdded e ->
                ( "load", P.encode e )

            CommitmentTypeAdded e ->
                ( "load", CT.encode e )

            CommitmentTypeRemoved e ->
                ( "load", Encode.string e )

            CommitmentAdded e ->
                ( "load", CM.encode e )

            EventTypeAdded e ->
                ( "load", ET.encode e )

            LinkedEventTypeToProcessType e ->
                ( "load", PTET.encode e )

            LinkedCommitmentTypeToProcessType e ->
                ( "load", PTCT.encode e )

            EventTypeRemoved e ->
                ( "load", Encode.string e )

            EventAdded e ->
                ( "load", E.encode e )

            ConnectionInitiated e ->
                ( "load"
                , Encode.object
                    [ ( "lastEventTime", Encode.int <| posixToMillis e.lastEventTime )
                    , ( "uuids", Encode.list Uuid.encode <| Set.toList e.uuids )
                    ]
                )

            GroupAdded e ->
                ( "load", G.encode e )

            GroupRemoved e ->
                ( "load", Encode.string e )

            IdentificationAdded e ->
                ( "load", I.encodeIdentification e )

            IdentificationRemoved e ->
                ( "load", Encode.string e )

            AgentTypeAdded e ->
                ( "load", AT.encode e )

            AgentTypeRemoved e ->
                ( "load", Encode.string e )

            AgentAdded e ->
                ( "load", A.encode e )

            AgentRemoved e ->
                ( "load", Uuid.encode e )

            IdentifierAdded e ->
                ( "load", I.encodeIdentifier e )
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
                                (Decode.field "load" PT.decoder)

                        "ProcessTypeRemoved" ->
                            Decode.map ProcessTypeRemoved
                                (Decode.field "load" Decode.string)

                        "ProcessAdded" ->
                            Decode.map ProcessAdded
                                (Decode.field "load" P.decoder)

                        "CommitmentTypeAdded" ->
                            Decode.map CommitmentTypeAdded
                                (Decode.field "load" CT.decoder)

                        "CommitmentTypeRemoved" ->
                            Decode.map CommitmentTypeRemoved
                                (Decode.field "load" Decode.string)

                        "CommitmentAdded" ->
                            Decode.map CommitmentAdded
                                (Decode.field "load" CM.decoder)

                        "EventTypeAdded" ->
                            Decode.map EventTypeAdded
                                (Decode.field "load" ET.decoder)

                        "LinkedEventTypeToProcessType" ->
                            Decode.map LinkedEventTypeToProcessType
                                (Decode.field "load" PTET.decoder)

                        "LinkedCommitmentTypeToProcessType" ->
                            Decode.map LinkedCommitmentTypeToProcessType
                                (Decode.field "load" PTCT.decoder)

                        "EventTypeRemoved" ->
                            Decode.map EventTypeRemoved
                                (Decode.field "load" Decode.string)

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

                        "IdentificationAdded" ->
                            Decode.map IdentificationAdded
                                (Decode.field "load" I.identificationDecoder)

                        "IdentificationRemoved" ->
                            Decode.map IdentificationRemoved
                                (Decode.field "load" Decode.string)

                        "AgentTypeAdded" ->
                            Decode.map AgentTypeAdded
                                (Decode.field "load" AT.decoder)

                        "AgentTypeRemoved" ->
                            Decode.map AgentTypeRemoved
                                (Decode.field "load" Decode.string)

                        "AgentAdded" ->
                            Decode.map AgentAdded
                                (Decode.field "load" A.decoder)

                        "AgentRemoved" ->
                            Decode.map AgentRemoved
                                (Decode.field "load" Uuid.decoder)

                        "IdentifierAdded" ->
                            Decode.map IdentifierAdded
                                (Decode.field "load" I.identifierDecoder)

                        _ ->
                            Decode.fail <| "Unknown Event type: " ++ t
                )
        )
