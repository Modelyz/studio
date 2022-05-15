port module Event exposing (Event(..), EventBase, base, compare, decodelist, decoder, encode, exceptCI, getTime, readEvents, storeEvents, storeEventsToSend)

import DictSet as Set
import EventFlow exposing (EventFlow, decoder)
import IOStatus exposing (IOStatus(..))
import Json.Decode as Decode exposing (Decoder, andThen, decodeValue)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.Commitment as C exposing (Commitment)
import REA.CommitmentType as CT exposing (CommitmentType)
import REA.Entity exposing (Entity)
import REA.Event as E
import REA.EventType as ET exposing (EventType)
import REA.Identifier as Identifier exposing (Identifier)
import REA.Identifier.Portion as Portion exposing (Portion)
import REA.Process as P exposing (Process)
import REA.ProcessType as PT exposing (ProcessType)
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
    , posixtime : Time.Posix
    , flow : EventFlow
    }


type Event
    = ConnectionInitiated { lastEventTime : Time.Posix, uuids : Set.DictSet String Uuid } EventBase
    | ProcessTypeChanged { ptype : ProcessType } EventBase
    | ProcessTypeRemoved { ptype : String } EventBase
    | ProcessAdded { type_ : String, name : String } EventBase
    | CommitmentTypeAdded { commitmentType : CommitmentType } EventBase
    | CommitmentTypeRemoved { commitmentType : CommitmentType } EventBase
    | CommitmentAdded { process : Process, commitment : Commitment } EventBase
    | EventTypeAdded { eventType : EventType } EventBase
    | EventTypeRemoved { eventType : EventType } EventBase
    | EventAdded { process : Process, event : E.Event } EventBase
    | LinkedEventTypeToProcessType { etype : String, ptype : String } EventBase
    | LinkedCommitmentTypeToProcessType { ctype : String, ptype : String } EventBase
    | GroupAdded { name : String, entity : Entity } EventBase
    | GroupRemoved { name : String, entity : Entity } EventBase
    | IdentifierAdded { name : String, entity : Entity, unique : Bool, mandatory : Bool, format : List Portion } EventBase
    | IdentifierRemoved String EventBase --TODO also simplify other *Removed


base : Event -> EventBase
base event =
    case event of
        ProcessTypeChanged _ b ->
            b

        ProcessTypeRemoved _ b ->
            b

        ProcessAdded _ b ->
            b

        CommitmentTypeAdded _ b ->
            b

        CommitmentTypeRemoved _ b ->
            b

        CommitmentAdded _ b ->
            b

        EventTypeAdded _ b ->
            b

        EventTypeRemoved _ b ->
            b

        EventAdded _ b ->
            b

        LinkedEventTypeToProcessType _ b ->
            b

        LinkedCommitmentTypeToProcessType _ b ->
            b

        ConnectionInitiated _ b ->
            b

        GroupAdded _ b ->
            b

        GroupRemoved _ b ->
            b

        IdentifierAdded _ b ->
            b

        IdentifierRemoved _ b ->
            b


compare : Event -> Int
compare =
    getTime >> posixToMillis


getTime : Event -> Time.Posix
getTime =
    base >> .posixtime


exceptCI : List Event -> List Event
exceptCI es =
    List.filter
        (\e ->
            case e of
                ConnectionInitiated _ _ ->
                    False

                _ ->
                    True
        )
        es



-- Event constructors


processTypeChanged : Uuid -> Time.Posix -> EventFlow -> ProcessType -> Event
processTypeChanged uuid posixtime flow ptype =
    ProcessTypeChanged
        { ptype = ptype
        }
        (EventBase uuid posixtime flow)


processTypeRemoved : Uuid -> Time.Posix -> EventFlow -> String -> Event
processTypeRemoved uuid posixtime flow ptype =
    ProcessTypeRemoved
        { ptype = ptype
        }
        (EventBase uuid posixtime flow)


processAdded : Uuid -> Time.Posix -> EventFlow -> String -> String -> Event
processAdded uuid posixtime flow name ptype =
    ProcessAdded
        { name = name
        , type_ = ptype
        }
        (EventBase uuid posixtime flow)


commitmentTypeRemoved : Uuid -> Time.Posix -> EventFlow -> CommitmentType -> Event
commitmentTypeRemoved uuid posixtime flow commitmentType =
    CommitmentTypeRemoved
        { commitmentType = commitmentType
        }
        (EventBase uuid posixtime flow)


commitmentTypeAdded : Uuid -> Time.Posix -> EventFlow -> CommitmentType -> Event
commitmentTypeAdded uuid posixtime flow commitmentType =
    CommitmentTypeAdded
        { commitmentType = commitmentType
        }
        (EventBase uuid posixtime flow)


commitmentAdded : Uuid -> Time.Posix -> EventFlow -> Process -> Commitment -> Event
commitmentAdded uuid posixtime flow process commitment =
    CommitmentAdded
        { commitment = commitment
        , process = process
        }
        (EventBase uuid posixtime flow)


eventTypeRemoved : Uuid -> Time.Posix -> EventFlow -> EventType -> Event
eventTypeRemoved uuid posixtime flow eventType =
    EventTypeRemoved
        { eventType = eventType
        }
        (EventBase uuid posixtime flow)


linkedEventTypeToProcessType : Uuid -> Time.Posix -> EventFlow -> String -> String -> Event
linkedEventTypeToProcessType uuid posixtime flow etype ptype =
    LinkedEventTypeToProcessType
        { etype = etype
        , ptype = ptype
        }
        (EventBase uuid posixtime flow)


linkedCommitmentTypeToProcessType : Uuid -> Time.Posix -> EventFlow -> String -> String -> Event
linkedCommitmentTypeToProcessType uuid posixtime flow ctype ptype =
    LinkedCommitmentTypeToProcessType
        { ctype = ctype
        , ptype = ptype
        }
        (EventBase uuid posixtime flow)


eventTypeAdded : Uuid -> Time.Posix -> EventFlow -> EventType -> Event
eventTypeAdded uuid posixtime flow eventType =
    EventTypeAdded
        { eventType = eventType
        }
        (EventBase uuid posixtime flow)


eventAdded : Uuid -> Time.Posix -> EventFlow -> Process -> E.Event -> Event
eventAdded uuid posixtime flow process event =
    EventAdded
        { event = event
        , process = process
        }
        (EventBase uuid posixtime flow)


connectionInitiated : Uuid -> Time.Posix -> EventFlow -> Time.Posix -> Set.DictSet String Uuid -> Event
connectionInitiated uuid posixtime flow lastEventTime uuids =
    ConnectionInitiated
        { lastEventTime = lastEventTime
        , uuids = uuids
        }
        (EventBase uuid posixtime flow)


groupAdded : Uuid -> Time.Posix -> EventFlow -> String -> Entity -> Event
groupAdded uuid posixtime flow name entity =
    GroupAdded
        { name = name
        , entity = entity
        }
        (EventBase uuid posixtime flow)


groupRemoved : Uuid -> Time.Posix -> EventFlow -> String -> Entity -> Event
groupRemoved uuid posixtime flow name entity =
    GroupRemoved
        { name = name
        , entity = entity
        }
        (EventBase uuid posixtime flow)


identifierAdded : Uuid -> Time.Posix -> EventFlow -> String -> Entity -> Bool -> Bool -> List Portion -> Event
identifierAdded uuid posixtime flow name entity unique mandatory format =
    IdentifierAdded
        { name = name
        , entity = entity
        , unique = unique
        , mandatory = mandatory
        , format = format
        }
        (EventBase uuid posixtime flow)


identifierRemoved : Uuid -> Time.Posix -> EventFlow -> String -> Event
identifierRemoved uuid posixtime flow name =
    IdentifierRemoved name (EventBase uuid posixtime flow)



-- JSON encoding / decoding


encode : Event -> Encode.Value
encode event =
    case event of
        ProcessTypeChanged e b ->
            Encode.object
                [ ( "type", Encode.string "ProcessTypeChanged" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "ptype", PT.encode e.ptype )
                ]

        ProcessTypeRemoved e b ->
            Encode.object
                [ ( "type", Encode.string "ProcessTypeRemoved" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "ptype", Encode.string e.ptype )
                ]

        ProcessAdded e b ->
            Encode.object
                [ ( "type", Encode.string "ProcessAdded" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "name", Encode.string e.name )
                , ( "type_", Encode.string e.type_ )
                ]

        CommitmentTypeAdded e b ->
            Encode.object
                [ ( "type", Encode.string "CommitmentTypeAdded" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "commitmentType", CT.encode e.commitmentType )
                ]

        CommitmentTypeRemoved e b ->
            Encode.object
                [ ( "type", Encode.string "CommitmentTypeRemoved" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "commitmentType", CT.encode e.commitmentType )
                ]

        CommitmentAdded e b ->
            Encode.object
                [ ( "type", Encode.string "CommitmentAdded" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "process", P.encode e.process )
                , ( "commitment", C.encode e.commitment )
                ]

        EventTypeAdded e b ->
            Encode.object
                [ ( "type", Encode.string "EventTypeAdded" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "eventType", ET.encode e.eventType )
                ]

        LinkedEventTypeToProcessType e b ->
            Encode.object
                [ ( "type", Encode.string "LinkedEventTypeToProcessType" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "etype", Encode.string e.etype )
                , ( "ptype", Encode.string e.ptype )
                ]

        LinkedCommitmentTypeToProcessType e b ->
            Encode.object
                [ ( "type", Encode.string "LinkedCommitmentTypeToProcessType" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "ctype", Encode.string e.ctype )
                , ( "ptype", Encode.string e.ptype )
                ]

        EventTypeRemoved e b ->
            Encode.object
                [ ( "type", Encode.string "EventTypeRemoved" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "eventType", ET.encode e.eventType )
                ]

        EventAdded e b ->
            Encode.object
                [ ( "type", Encode.string "EventAdded" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "process", P.encode e.process )
                , ( "event", E.encode e.event )
                ]

        ConnectionInitiated e b ->
            Encode.object
                [ ( "type", Encode.string "ConnectionInitiated" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "lastEventTime", Encode.int <| posixToMillis e.lastEventTime )
                , ( "uuids", Encode.list Uuid.encode <| Set.toList e.uuids )
                ]

        GroupAdded e b ->
            Encode.object
                [ ( "type", Encode.string "GroupAdded" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "name", Encode.string e.name )
                , ( "entity", REA.Entity.encode e.entity )
                ]

        GroupRemoved e b ->
            Encode.object
                [ ( "type", Encode.string "GroupRemoved" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "name", Encode.string e.name )
                , ( "entity", REA.Entity.encode e.entity )
                ]

        IdentifierAdded e b ->
            Encode.object
                [ ( "type", Encode.string "IdentifierAdded" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "name", Encode.string e.name )
                , ( "entity", REA.Entity.encode e.entity )
                , ( "unique", Encode.bool e.unique )
                , ( "mandatory", Encode.bool e.mandatory )
                , ( "format", Encode.list Portion.encode e.format )
                ]

        IdentifierRemoved e b ->
            Encode.object
                [ ( "type", Encode.string "IdentifierRemoved" )
                , ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "name", Encode.string e )
                ]


decodelist : Decode.Value -> List Event
decodelist =
    Result.withDefault [] << decodeValue (Decode.list decoder)


decoder : Decoder Event
decoder =
    let
        toPosix : Int -> Decoder Time.Posix
        toPosix t =
            Decode.succeed (millisToPosix t)
    in
    Decode.field "type" Decode.string
        |> andThen
            (\s ->
                case s of
                    "ProcessTypeChanged" ->
                        Decode.map4 processTypeChanged
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "ptype" PT.decoder)

                    "ProcessTypeRemoved" ->
                        Decode.map4 processTypeRemoved
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "ptype" Decode.string)

                    "ProcessAdded" ->
                        Decode.map5 processAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "type_" Decode.string)

                    "CommitmentTypeAdded" ->
                        Decode.map4 commitmentTypeAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "commitmentType" CT.decoder)

                    "CommitmentTypeRemoved" ->
                        Decode.map4 commitmentTypeRemoved
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "commitmentType" CT.decoder)

                    "CommitmentAdded" ->
                        Decode.map5 commitmentAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "process" P.decoder)
                            (Decode.field "commitment" C.decoder)

                    "EventTypeAdded" ->
                        Decode.map4 eventTypeAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "eventType" ET.decoder)

                    "LinkedEventTypeToProcessType" ->
                        Decode.map5 linkedEventTypeToProcessType
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "etype" Decode.string)
                            (Decode.field "ptype" Decode.string)

                    "LinkedCommitmentTypeToProcessType" ->
                        Decode.map5 linkedCommitmentTypeToProcessType
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "ctype" Decode.string)
                            (Decode.field "ptype" Decode.string)

                    "EventTypeRemoved" ->
                        Decode.map4 eventTypeRemoved
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "eventType" ET.decoder)

                    "EventAdded" ->
                        Decode.map5 eventAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "process" P.decoder)
                            (Decode.field "event" E.decoder)

                    "ConnectionInitiated" ->
                        Decode.map5 connectionInitiated
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "lastEventTime" Decode.int |> andThen toPosix)
                            (Decode.field "uuids" (Decode.list Uuid.decoder) |> andThen (\xs -> Decode.succeed (Set.fromList Uuid.toString xs)))

                    "GroupAdded" ->
                        Decode.map5 groupAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "entity" REA.Entity.decoder)

                    "GroupRemoved" ->
                        Decode.map5 groupRemoved
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "entity" REA.Entity.decoder)

                    "IdentifierAdded" ->
                        Decode.map8 identifierAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "name" Decode.string)
                            (Decode.field "entity" REA.Entity.decoder)
                            (Decode.field "unique" Decode.bool)
                            (Decode.field "mandatory" Decode.bool)
                            (Decode.field "format" (Decode.list Portion.decoder))

                    "IdentifierRemoved" ->
                        Decode.map4 identifierRemoved
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "name" Decode.string)

                    _ ->
                        Decode.fail "Unknown Event type"
            )
