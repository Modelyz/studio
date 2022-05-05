port module Event exposing (Event(..), EventBase, base, compare, decodelist, decoder, encode, exceptCI, getTime, readEvents, storeEvents, storeEventsToSend)

import DictSet as Set
import EventFlow exposing (EventFlow, decoder)
import IOStatus exposing (IOStatus(..))
import Json.Decode as Decode exposing (Decoder, andThen, decodeValue)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.Commitment as C exposing (Commitment)
import REA.CommitmentType as CT exposing (CommitmentType)
import REA.Event as E
import REA.EventType as ET exposing (EventType)
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
    = ProcessTypeChanged { ptype : ProcessType } EventBase
    | ProcessTypeRemoved { ptype : String } EventBase
    | ProcessAdded { type_ : String, name : String } EventBase
    | CommitmentTypeAdded { commitmentType : CommitmentType } EventBase
    | CommitmentTypeRemoved { commitmentType : CommitmentType } EventBase
    | CommitmentAdded { process : Process, commitment : Commitment } EventBase
    | EventTypeAdded { eventType : EventType } EventBase
    | EventTypeRemoved { eventType : EventType } EventBase
    | EventAdded { process : Process, event : E.Event } EventBase
    | LinkedEventTypeToProcessType { etype : String, ptype : String } EventBase
    | ConnectionInitiated { lastEventTime : Time.Posix, uuids : Set.DictSet String Uuid } EventBase


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

        ConnectionInitiated _ b ->
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
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        }


processTypeRemoved : Uuid -> Time.Posix -> EventFlow -> String -> Event
processTypeRemoved uuid posixtime flow ptype =
    ProcessTypeRemoved
        { ptype = ptype
        }
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        }


processAdded : Uuid -> Time.Posix -> EventFlow -> String -> String -> Event
processAdded uuid posixtime flow name ptype =
    ProcessAdded
        { name = name
        , type_ = ptype
        }
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        }


commitmentTypeRemoved : Uuid -> Time.Posix -> EventFlow -> CommitmentType -> Event
commitmentTypeRemoved uuid posixtime flow commitmentType =
    CommitmentTypeRemoved
        { commitmentType = commitmentType
        }
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        }


commitmentTypeAdded : Uuid -> Time.Posix -> EventFlow -> CommitmentType -> Event
commitmentTypeAdded uuid posixtime flow commitmentType =
    CommitmentTypeAdded
        { commitmentType = commitmentType
        }
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        }


commitmentAdded : Uuid -> Time.Posix -> EventFlow -> Process -> Commitment -> Event
commitmentAdded uuid posixtime flow process commitment =
    CommitmentAdded
        { commitment = commitment
        , process = process
        }
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        }


eventTypeRemoved : Uuid -> Time.Posix -> EventFlow -> EventType -> Event
eventTypeRemoved uuid posixtime flow eventType =
    EventTypeRemoved
        { eventType = eventType
        }
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        }


linkedEventTypeToProcessType : Uuid -> Time.Posix -> EventFlow -> String -> String -> Event
linkedEventTypeToProcessType uuid posixtime flow etype ptype =
    LinkedEventTypeToProcessType
        { etype = etype
        , ptype = ptype
        }
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        }


eventTypeAdded : Uuid -> Time.Posix -> EventFlow -> EventType -> Event
eventTypeAdded uuid posixtime flow eventType =
    EventTypeAdded
        { eventType = eventType
        }
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        }


eventAdded : Uuid -> Time.Posix -> EventFlow -> Process -> E.Event -> Event
eventAdded uuid posixtime flow process event =
    EventAdded
        { event = event
        , process = process
        }
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        }


connectionInitiated : Uuid -> Time.Posix -> EventFlow -> Time.Posix -> Set.DictSet String Uuid -> Event
connectionInitiated uuid posixtime flow lastEventTime uuids =
    ConnectionInitiated
        { lastEventTime = lastEventTime
        , uuids = uuids
        }
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        }



-- JSON encoding / decoding


encode : Event -> Encode.Value
encode event =
    case event of
        ProcessTypeChanged e b ->
            Encode.object
                [ ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "type", Encode.string "ProcessTypeChanged" )
                , ( "ptype", PT.encode e.ptype )
                ]

        ProcessTypeRemoved e b ->
            Encode.object
                [ ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "type", Encode.string "ProcessTypeRemoved" )
                , ( "ptype", Encode.string e.ptype )
                ]

        ProcessAdded e b ->
            Encode.object
                [ ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "type", Encode.string "ProcessAdded" )
                , ( "name", Encode.string e.name )
                , ( "type_", Encode.string e.type_ )
                ]

        CommitmentTypeAdded e b ->
            Encode.object
                [ ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "type", Encode.string "CommitmentTypeAdded" )
                , ( "commitmentType", CT.encode e.commitmentType )
                ]

        CommitmentTypeRemoved e b ->
            Encode.object
                [ ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "type", Encode.string "CommitmentTypeRemoved" )
                , ( "commitmentType", CT.encode e.commitmentType )
                ]

        CommitmentAdded e b ->
            Encode.object
                [ ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "type", Encode.string "CommitmentAdded" )
                , ( "process", P.encode e.process )
                , ( "commitment", C.encode e.commitment )
                ]

        EventTypeAdded e b ->
            Encode.object
                [ ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "type", Encode.string "EventTypeAdded" )
                , ( "eventType", ET.encode e.eventType )
                ]

        LinkedEventTypeToProcessType e b ->
            Encode.object
                [ ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "type", Encode.string "LinkedEventTypeToProcessType" )
                , ( "etype", Encode.string e.etype )
                , ( "ptype", Encode.string e.ptype )
                ]

        EventTypeRemoved e b ->
            Encode.object
                [ ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "type", Encode.string "EventTypeRemoved" )
                , ( "eventType", ET.encode e.eventType )
                ]

        EventAdded e b ->
            Encode.object
                [ ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "type", Encode.string "EventAdded" )
                , ( "process", P.encode e.process )
                , ( "event", E.encode e.event )
                ]

        ConnectionInitiated e b ->
            Encode.object
                [ ( "uuid", Uuid.encode b.uuid )
                , ( "posixtime", Encode.int <| posixToMillis b.posixtime )
                , ( "flow", EventFlow.encode b.flow )
                , ( "type", Encode.string "ConnectionInitiated" )
                , ( "lastEventTime", Encode.int <| posixToMillis e.lastEventTime )
                , ( "uuids", Encode.list Uuid.encode <| Set.toList e.uuids )
                ]


decodelist : Decode.Value -> List Event
decodelist =
    Result.withDefault [] << decodeValue (Decode.list decoder)


decoder : Decoder Event
decoder =
    let
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

                    _ ->
                        Decode.fail "Unknown Event type"
            )
