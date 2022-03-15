port module Event exposing (Event(..), base, compare, decodelist, decoder, encode, getTime, readEvents, storeEvents, storeEventsToSend)

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


type alias EventBase a =
    { a
        | uuid : Uuid
        , posixtime : Time.Posix
        , flow : EventFlow
    }


type Event
    = ProcessTypeChanged (EventBase { ptype : ProcessType })
    | ProcessTypeRemoved (EventBase { ptype : String })
    | ProcessAdded (EventBase { type_ : String, name : String })
    | CommitmentTypeAdded (EventBase { commitmentType : CommitmentType })
    | CommitmentTypeRemoved (EventBase { commitmentType : CommitmentType })
    | CommitmentAdded (EventBase { process : Process, commitment : Commitment })
    | EventTypeAdded (EventBase { eventType : EventType })
    | EventTypeRemoved (EventBase { eventType : EventType })
    | EventAdded (EventBase { process : Process, event : E.Event })
    | LinkedEventTypeToProcessType (EventBase { etype : String, ptype : String })
    | ConnectionInitiated (EventBase { lastEventTime : Time.Posix, sessionUuid : Uuid, uuids : Set.DictSet String Uuid })


base : Event -> EventBase {}
base event =
    case event of
        ProcessTypeChanged e ->
            { uuid = e.uuid, posixtime = e.posixtime, flow = e.flow }

        ProcessTypeRemoved e ->
            { uuid = e.uuid, posixtime = e.posixtime, flow = e.flow }

        ProcessAdded e ->
            { uuid = e.uuid, posixtime = e.posixtime, flow = e.flow }

        CommitmentTypeAdded e ->
            { uuid = e.uuid, posixtime = e.posixtime, flow = e.flow }

        CommitmentTypeRemoved e ->
            { uuid = e.uuid, posixtime = e.posixtime, flow = e.flow }

        CommitmentAdded e ->
            { uuid = e.uuid, posixtime = e.posixtime, flow = e.flow }

        EventTypeAdded e ->
            { uuid = e.uuid, posixtime = e.posixtime, flow = e.flow }

        EventTypeRemoved e ->
            { uuid = e.uuid, posixtime = e.posixtime, flow = e.flow }

        EventAdded e ->
            { uuid = e.uuid, posixtime = e.posixtime, flow = e.flow }

        LinkedEventTypeToProcessType e ->
            { uuid = e.uuid, posixtime = e.posixtime, flow = e.flow }

        ConnectionInitiated e ->
            { uuid = e.uuid, posixtime = e.posixtime, flow = e.flow }


compare : Event -> Int
compare =
    getTime >> posixToMillis


getTime : Event -> Time.Posix
getTime =
    base >> .posixtime



-- Event constructors


processTypeChanged : Uuid -> Time.Posix -> EventFlow -> ProcessType -> Event
processTypeChanged uuid posixtime flow ptype =
    ProcessTypeChanged
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        , ptype = ptype
        }


processTypeRemoved : Uuid -> Time.Posix -> EventFlow -> String -> Event
processTypeRemoved uuid posixtime flow ptype =
    ProcessTypeRemoved
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        , ptype = ptype
        }


processAdded : Uuid -> Time.Posix -> EventFlow -> String -> String -> Event
processAdded uuid posixtime flow name ptype =
    ProcessAdded
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        , name = name
        , type_ = ptype
        }


commitmentTypeRemoved : Uuid -> Time.Posix -> EventFlow -> CommitmentType -> Event
commitmentTypeRemoved uuid posixtime flow commitmentType =
    CommitmentTypeRemoved
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        , commitmentType = commitmentType
        }


commitmentTypeAdded : Uuid -> Time.Posix -> EventFlow -> CommitmentType -> Event
commitmentTypeAdded uuid posixtime flow commitmentType =
    CommitmentTypeAdded
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        , commitmentType = commitmentType
        }


commitmentAdded : Uuid -> Time.Posix -> EventFlow -> Process -> Commitment -> Event
commitmentAdded uuid posixtime flow process commitment =
    CommitmentAdded
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        , commitment = commitment
        , process = process
        }


eventTypeRemoved : Uuid -> Time.Posix -> EventFlow -> EventType -> Event
eventTypeRemoved uuid posixtime flow eventType =
    EventTypeRemoved
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        , eventType = eventType
        }


linkedEventTypeToProcessType : Uuid -> Time.Posix -> EventFlow -> String -> String -> Event
linkedEventTypeToProcessType uuid posixtime flow etype ptype =
    LinkedEventTypeToProcessType
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        , etype = etype
        , ptype = ptype
        }


eventTypeAdded : Uuid -> Time.Posix -> EventFlow -> EventType -> Event
eventTypeAdded uuid posixtime flow eventType =
    EventTypeAdded
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        , eventType = eventType
        }


eventAdded : Uuid -> Time.Posix -> EventFlow -> Process -> E.Event -> Event
eventAdded uuid posixtime flow process event =
    EventAdded
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        , event = event
        , process = process
        }


connectionInitiated : Uuid -> Time.Posix -> EventFlow -> Time.Posix -> Uuid -> Set.DictSet String Uuid -> Event
connectionInitiated uuid posixtime flow lastEventTime sessionUuid uuids =
    ConnectionInitiated
        { uuid = uuid
        , posixtime = posixtime
        , flow = flow
        , lastEventTime = lastEventTime
        , sessionUuid = sessionUuid
        , uuids = uuids
        }


encode : Event -> Encode.Value
encode event =
    case event of
        ProcessTypeChanged e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "flow", EventFlow.encode e.flow )
                , ( "type", Encode.string "ProcessTypeChanged" )
                , ( "ptype", PT.encode e.ptype )
                ]

        ProcessTypeRemoved e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "flow", EventFlow.encode e.flow )
                , ( "type", Encode.string "ProcessTypeRemoved" )
                , ( "ptype", Encode.string e.ptype )
                ]

        ProcessAdded e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "flow", EventFlow.encode e.flow )
                , ( "type", Encode.string "ProcessAdded" )
                , ( "name", Encode.string e.name )
                , ( "type_", Encode.string e.type_ )
                ]

        CommitmentTypeAdded e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "flow", EventFlow.encode e.flow )
                , ( "type", Encode.string "CommitmentTypeAdded" )
                , ( "commitmentType", CT.encode e.commitmentType )
                ]

        CommitmentTypeRemoved e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "flow", EventFlow.encode e.flow )
                , ( "type", Encode.string "CommitmentTypeRemoved" )
                , ( "commitmentType", CT.encode e.commitmentType )
                ]

        CommitmentAdded e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "flow", EventFlow.encode e.flow )
                , ( "type", Encode.string "CommitmentAdded" )
                , ( "process", P.encode e.process )
                , ( "commitment", C.encode e.commitment )
                ]

        EventTypeAdded e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "flow", EventFlow.encode e.flow )
                , ( "type", Encode.string "EventTypeAdded" )
                , ( "eventType", ET.encode e.eventType )
                ]

        LinkedEventTypeToProcessType e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "flow", EventFlow.encode e.flow )
                , ( "type", Encode.string "LinkedEventTypeToProcessType" )
                , ( "etype", Encode.string e.etype )
                , ( "ptype", Encode.string e.ptype )
                ]

        EventTypeRemoved e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "flow", EventFlow.encode e.flow )
                , ( "type", Encode.string "EventTypeRemoved" )
                , ( "eventType", ET.encode e.eventType )
                ]

        EventAdded e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "flow", EventFlow.encode e.flow )
                , ( "type", Encode.string "EventAdded" )
                , ( "process", P.encode e.process )
                , ( "event", E.encode e.event )
                ]

        ConnectionInitiated e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "flow", EventFlow.encode e.flow )
                , ( "type", Encode.string "ConnectionInitiated" )
                , ( "lastEventTime", Encode.int <| posixToMillis e.lastEventTime )
                , ( "sessionUuid", Uuid.encode e.sessionUuid )
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
                        Decode.map6 connectionInitiated
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "flow" EventFlow.decoder)
                            (Decode.field "lastEventTime" Decode.int |> andThen toPosix)
                            (Decode.field "sessionUuid" Uuid.decoder)
                            (Decode.field "uuids" (Decode.list Uuid.decoder) |> andThen (\xs -> Decode.succeed (Set.fromList Uuid.toString xs)))

                    _ ->
                        Decode.fail "Unknown Event type"
            )
