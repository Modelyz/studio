port module Event exposing (Event(..), compare, decodelist, decoder, encode, getTime, readEvents, storeEvents, storeEventsToSend, unbox)

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
    | ConnectionInitiated (EventBase { lastEventTime : Time.Posix, sessionUuid : Uuid })
    | AckReceived (EventBase { origin : Uuid })


processTypeChanged : Uuid -> Time.Posix -> ProcessType -> Event
processTypeChanged uuid posixtime ptype =
    ProcessTypeChanged
        { uuid = uuid
        , posixtime = posixtime
        , ptype = ptype
        }


processTypeRemoved : Uuid -> Time.Posix -> String -> Event
processTypeRemoved uuid posixtime ptype =
    ProcessTypeRemoved
        { uuid = uuid
        , posixtime = posixtime
        , ptype = ptype
        }


unbox : Event -> EventBase {}
unbox event =
    case event of
        ProcessTypeChanged e ->
            { uuid = e.uuid, posixtime = e.posixtime }

        ProcessTypeRemoved e ->
            { uuid = e.uuid, posixtime = e.posixtime }

        ProcessAdded e ->
            { uuid = e.uuid, posixtime = e.posixtime }

        CommitmentTypeAdded e ->
            { uuid = e.uuid, posixtime = e.posixtime }

        CommitmentTypeRemoved e ->
            { uuid = e.uuid, posixtime = e.posixtime }

        CommitmentAdded e ->
            { uuid = e.uuid, posixtime = e.posixtime }

        EventTypeAdded e ->
            { uuid = e.uuid, posixtime = e.posixtime }

        EventTypeRemoved e ->
            { uuid = e.uuid, posixtime = e.posixtime }

        EventAdded e ->
            { uuid = e.uuid, posixtime = e.posixtime }

        LinkedEventTypeToProcessType e ->
            { uuid = e.uuid, posixtime = e.posixtime }

        ConnectionInitiated e ->
            { uuid = e.uuid, posixtime = e.posixtime }

        AckReceived e ->
            { uuid = e.uuid, posixtime = e.posixtime }


compare : Event -> Int
compare =
    getTime >> posixToMillis


getTime : Event -> Time.Posix
getTime =
    unbox >> .posixtime



-- Event constructors


processAdded : Uuid -> Time.Posix -> String -> String -> Event
processAdded uuid posixtime pname ptype =
    ProcessAdded
        { uuid = uuid
        , posixtime = posixtime
        , name = pname
        , type_ = ptype
        }


commitmentTypeRemoved : Uuid -> Time.Posix -> CommitmentType -> Event
commitmentTypeRemoved uuid posixtime commitmentType =
    CommitmentTypeRemoved
        { uuid = uuid
        , posixtime = posixtime
        , commitmentType = commitmentType
        }


commitmentTypeAdded : Uuid -> Time.Posix -> CommitmentType -> Event
commitmentTypeAdded uuid posixtime commitmentType =
    CommitmentTypeAdded
        { uuid = uuid
        , posixtime = posixtime
        , commitmentType = commitmentType
        }


commitmentAdded : Uuid -> Time.Posix -> Process -> Commitment -> Event
commitmentAdded uuid posixtime process commitment =
    CommitmentAdded
        { uuid = uuid
        , posixtime = posixtime
        , commitment = commitment
        , process = process
        }


eventTypeRemoved : Uuid -> Time.Posix -> EventType -> Event
eventTypeRemoved uuid posixtime eventType =
    EventTypeRemoved
        { uuid = uuid
        , posixtime = posixtime
        , eventType = eventType
        }


linkedEventTypeToProcessType : Uuid -> Time.Posix -> String -> String -> Event
linkedEventTypeToProcessType uuid posixtime etype ptype =
    LinkedEventTypeToProcessType
        { uuid = uuid
        , posixtime = posixtime
        , etype = etype
        , ptype = ptype
        }


eventTypeAdded : Uuid -> Time.Posix -> EventType -> Event
eventTypeAdded uuid posixtime eventType =
    EventTypeAdded
        { uuid = uuid
        , posixtime = posixtime
        , eventType = eventType
        }


eventAdded : Uuid -> Time.Posix -> Process -> E.Event -> Event
eventAdded uuid posixtime process event =
    EventAdded
        { uuid = uuid
        , posixtime = posixtime
        , event = event
        , process = process
        }


connectionInitiated : Uuid -> Time.Posix -> Time.Posix -> Uuid -> Event
connectionInitiated uuid posixtime lastEventTime sessionUuid =
    ConnectionInitiated
        { uuid = uuid
        , posixtime = posixtime
        , lastEventTime = lastEventTime
        , sessionUuid = sessionUuid
        }


ackReceived : Uuid -> Time.Posix -> Uuid -> Event
ackReceived uuid posixtime origin =
    AckReceived
        { uuid = uuid
        , posixtime = posixtime
        , origin = origin
        }


encode : Event -> Encode.Value
encode event =
    case event of
        ProcessTypeChanged e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "type", Encode.string "ProcessTypeChanged" )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "ptype", PT.encode e.ptype )
                ]

        ProcessTypeRemoved e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "type", Encode.string "ProcessTypeRemoved" )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "ptype", Encode.string e.ptype )
                ]

        ProcessAdded e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "type", Encode.string "ProcessAdded" )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "name", Encode.string e.name )
                , ( "type_", Encode.string e.type_ )
                ]

        CommitmentTypeAdded e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "type", Encode.string "CommitmentTypeAdded" )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "commitmentType", CT.encode e.commitmentType )
                ]

        CommitmentTypeRemoved e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "type", Encode.string "CommitmentTypeRemoved" )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "commitmentType", CT.encode e.commitmentType )
                ]

        CommitmentAdded e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "type", Encode.string "CommitmentAdded" )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "process", P.encode e.process )
                , ( "commitment", C.encode e.commitment )
                ]

        EventTypeAdded e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "type", Encode.string "EventTypeAdded" )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "eventType", ET.encode e.eventType )
                ]

        LinkedEventTypeToProcessType e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "type", Encode.string "LinkedEventTypeToProcessType" )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "etype", Encode.string e.etype )
                , ( "ptype", Encode.string e.ptype )
                ]

        EventTypeRemoved e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "type", Encode.string "EventTypeRemoved" )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "eventType", ET.encode e.eventType )
                ]

        EventAdded e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "type", Encode.string "EventAdded" )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "process", P.encode e.process )
                , ( "event", E.encode e.event )
                ]

        ConnectionInitiated e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "type", Encode.string "ConnectionInitiated" )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "lastEventTime", Encode.int <| posixToMillis e.lastEventTime )
                , ( "sessionUuid", Uuid.encode e.sessionUuid )
                ]

        AckReceived e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "type", Encode.string "AckReceived" )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "origin", Uuid.encode e.origin )
                ]


decodelist : Decode.Value -> List Event
decodelist =
    Result.withDefault [] << decodeValue (Decode.list decoder)


decoder : Decoder Event
decoder =
    let
        toPosix t =
            millisToPosix t |> Decode.succeed
    in
    Decode.field "type" Decode.string
        |> andThen
            (\s ->
                case s of
                    "ProcessTypeChanged" ->
                        Decode.map3 processTypeChanged
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "ptype" PT.decoder)

                    "ProcessTypeRemoved" ->
                        Decode.map3 processTypeRemoved
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "ptype" Decode.string)

                    "ProcessAdded" ->
                        Decode.map4 processAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "name" Decode.string)
                            (Decode.field "type_" Decode.string)

                    "CommitmentTypeAdded" ->
                        Decode.map3 commitmentTypeAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "commitmentType" CT.decoder)

                    "CommitmentTypeRemoved" ->
                        Decode.map3 commitmentTypeRemoved
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "commitmentType" CT.decoder)

                    "CommitmentAdded" ->
                        Decode.map4 commitmentAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "process" P.decoder)
                            (Decode.field "commitment" C.decoder)

                    "EventTypeAdded" ->
                        Decode.map3 eventTypeAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "eventType" ET.decoder)

                    "LinkedEventTypeToProcessType" ->
                        Decode.map4 linkedEventTypeToProcessType
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "etype" Decode.string)
                            (Decode.field "ptype" Decode.string)

                    "EventTypeRemoved" ->
                        Decode.map3 eventTypeRemoved
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "eventType" ET.decoder)

                    "EventAdded" ->
                        Decode.map4 eventAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "process" P.decoder)
                            (Decode.field "event" E.decoder)

                    "ConnectionInitiated" ->
                        Decode.map4 connectionInitiated
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "lastEventTime" Decode.int |> andThen toPosix)
                            (Decode.field "sessionUuid" Uuid.decoder)

                    "AckReceived" ->
                        Decode.map3 ackReceived
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "origin" Uuid.decoder)

                    _ ->
                        Decode.fail "Unknown Event type"
            )
