port module ES exposing (Event(..), State, aggregate, compare, currentProcessType, decodelist, decoder, encode, getCommitments, getEvents, getProcess, getProcessType, getTime, new, readEvents, sendEvents, storeEvents, storeEventsToSend, wsConnect)

import Browser.Navigation as Nav
import DictSet as Set exposing (DictSet)
import Json.Decode as Decode exposing (Decoder, andThen, decodeValue)
import Json.Encode as Encode
import Prng.Uuid as Uuid exposing (Uuid)
import REA.Commitment as C exposing (Commitment)
import REA.CommitmentType as CT exposing (CommitmentType)
import REA.Event as E
import REA.EventType as ET exposing (EventType)
import REA.Process as P exposing (Process)
import REA.ProcessCommitments as PC exposing (ProcessCommitments)
import REA.ProcessEvents as PE exposing (ProcessEvents)
import REA.ProcessType as PT exposing (ProcessType)
import REA.ProcessTypeCommitmentType as PTCT exposing (ProcessTypeCommitmentType)
import REA.ProcessTypeEventType as PTET exposing (ProcessTypeEventType)
import Random.Pcg.Extended exposing (Seed)
import Result exposing (Result(..))
import Route exposing (Route)
import Status exposing (ESStatus(..), WSStatus(..))
import Time exposing (millisToPosix, posixToMillis)



-- read events from IDB


port readEvents : Encode.Value -> Cmd msg



-- store events to IDB then send to WS


port storeEvents : Encode.Value -> Cmd msg



-- only store to IDB


port storeEventsToSend : Encode.Value -> Cmd msg



-- send events through WS


port sendEvents : String -> Cmd msg


port wsConnect : () -> Cmd msg



-- application/user events --


type Event
    = ProcessTypeChanged
        { uuid : Uuid.Uuid
        , posixtime : Time.Posix
        , ptype : ProcessType
        }
    | ProcessTypeRemoved
        { uuid : Uuid.Uuid
        , posixtime : Time.Posix
        , ptype : String
        }
    | ProcessAdded
        { uuid : Uuid.Uuid
        , posixtime : Time.Posix
        , type_ : String
        }
    | CommitmentTypeAdded
        { uuid : Uuid.Uuid
        , posixtime : Time.Posix
        , commitmentType : CommitmentType
        }
    | CommitmentTypeRemoved
        { uuid : Uuid.Uuid
        , posixtime : Time.Posix
        , commitmentType : CommitmentType
        }
    | CommitmentAdded
        { uuid : Uuid.Uuid
        , posixtime : Time.Posix
        , process : Process
        , commitment : Commitment
        }
    | EventTypeAdded
        { uuid : Uuid.Uuid
        , posixtime : Time.Posix
        , eventType : EventType
        }
    | EventTypeRemoved
        { uuid : Uuid.Uuid
        , posixtime : Time.Posix
        , eventType : EventType
        }
    | EventAdded
        { uuid : Uuid.Uuid
        , posixtime : Time.Posix
        , process : Process
        , event : E.Event
        }
    | LinkedEventTypeToProcessType
        { uuid : Uuid.Uuid
        , posixtime : Time.Posix
        , etype : String
        , ptype : String
        }
    | ConnectionInitiated
        { uuid : Uuid.Uuid
        , posixtime : Time.Posix
        , lastEventTime : Time.Posix
        , sessionUuid : Uuid.Uuid

        -- TODO add auth here
        }



-- global application state --
-- TODO : différencier le state et le model ? le state est l'agrégation des evenements, le model est ce qui représente l'ui. (alors le state serait dans le model)


type alias State =
    -- ui model related
    { currentSeed : Seed
    , navkey : Nav.Key
    , route : Route
    , esstatus : ESStatus
    , wsstatus : WSStatus
    , timeoutReconnect : Int

    -- input related
    , inputProcessType : ProcessType
    , inputCommitmentType : String
    , inputCommitmentTypeProcessTypes : DictSet String String
    , inputEventType : String
    , inputEventTypeProcessTypes : DictSet String String

    -- ES related
    , lastEventTime : Time.Posix

    -- session related
    , sessionUuid : Maybe Uuid

    -- REA state related
    , processTypes : DictSet String ProcessType
    , processes : DictSet Int Process
    , commitmentTypes : DictSet String CommitmentType
    , commitments : DictSet Int Commitment
    , process_commitments : DictSet Int ProcessCommitments
    , eventTypes : DictSet String EventType
    , events : DictSet Int E.Event
    , process_events : DictSet Int ProcessEvents
    , processType_commitmentTypes : DictSet String ProcessTypeCommitmentType
    , processType_eventTypes : DictSet String ProcessTypeEventType
    }


new : Seed -> Nav.Key -> Route -> State
new seed key route =
    { currentSeed = seed
    , navkey = key
    , route = route
    , esstatus = ESReading
    , wsstatus = WSInit
    , timeoutReconnect = 1
    , inputProcessType = { name = "" }
    , inputCommitmentType = ""
    , inputEventType = ""
    , inputCommitmentTypeProcessTypes = Set.empty identity
    , inputEventTypeProcessTypes = Set.empty identity
    , processTypes = Set.empty PT.compare
    , processes = Set.empty P.compare
    , process_commitments = Set.empty PC.compare
    , commitmentTypes = Set.empty CT.compare
    , commitments = Set.empty C.compare
    , eventTypes = Set.empty ET.compare
    , events = Set.empty E.compare
    , lastEventTime = millisToPosix 0
    , sessionUuid = Nothing
    , process_events = Set.empty PE.compare
    , processType_commitmentTypes = Set.empty PTCT.compare
    , processType_eventTypes = Set.empty PTET.compare
    }


compare : Event -> Int
compare event =
    case event of
        ProcessTypeChanged pt ->
            posixToMillis pt.posixtime

        ProcessTypeRemoved pt ->
            posixToMillis pt.posixtime

        ProcessAdded p ->
            posixToMillis p.posixtime

        CommitmentTypeAdded ct ->
            posixToMillis ct.posixtime

        CommitmentTypeRemoved ct ->
            posixToMillis ct.posixtime

        CommitmentAdded c ->
            posixToMillis c.posixtime

        EventTypeAdded et ->
            posixToMillis et.posixtime

        EventTypeRemoved et ->
            posixToMillis et.posixtime

        EventAdded e ->
            posixToMillis e.posixtime

        LinkedEventTypeToProcessType e ->
            posixToMillis e.posixtime

        ConnectionInitiated e ->
            posixToMillis e.posixtime


getTime : Event -> Time.Posix
getTime event =
    -- Maybe we could avoid redecoding after storing??
    case event of
        ProcessTypeChanged pt ->
            pt.posixtime

        ProcessTypeRemoved pt ->
            pt.posixtime

        ProcessAdded p ->
            p.posixtime

        CommitmentTypeAdded ct ->
            ct.posixtime

        CommitmentTypeRemoved ct ->
            ct.posixtime

        CommitmentAdded c ->
            c.posixtime

        EventTypeAdded et ->
            et.posixtime

        EventTypeRemoved et ->
            et.posixtime

        EventAdded e ->
            e.posixtime

        LinkedEventTypeToProcessType e ->
            e.posixtime

        ConnectionInitiated e ->
            e.posixtime


getProcess : State -> String -> Maybe Process
getProcess state str =
    -- TODO : move to Page/Process?
    Uuid.fromString str
        |> Maybe.andThen
            (\uuid ->
                Set.filter
                    (\p -> p.uuid == uuid)
                    state.processes
                    |> Set.values
                    |> List.head
            )


getCommitments : State -> Process -> DictSet Int Commitment
getCommitments state process =
    Set.filter (\pc -> pc.process.uuid == process.uuid) state.process_commitments
        |> Set.map C.compare (\pc -> pc.commitment)


getEvents : State -> Process -> DictSet Int E.Event
getEvents state process =
    Set.filter (\pe -> pe.process.uuid == process.uuid) state.process_events
        |> Set.map E.compare (\pe -> pe.event)



--- evolve the state given an event


aggregate : Event -> State -> State
aggregate event state =
    case event of
        ProcessTypeChanged e ->
            { state
                | processTypes = Set.insert e.ptype state.processTypes
                , lastEventTime = e.posixtime
            }

        ProcessTypeRemoved e ->
            { state
                | processTypes = Set.filter (\pt -> pt.name /= e.ptype) state.processTypes
                , lastEventTime = e.posixtime
            }

        ProcessAdded e ->
            { state
                | processes = Set.insert e state.processes
                , lastEventTime = e.posixtime
            }

        CommitmentTypeAdded e ->
            { state
                | commitmentTypes = Set.insert e.commitmentType state.commitmentTypes
                , lastEventTime = e.posixtime
            }

        CommitmentTypeRemoved e ->
            { state
                | commitmentTypes = Set.remove e.commitmentType state.commitmentTypes
                , lastEventTime = e.posixtime
            }

        CommitmentAdded e ->
            { state
                | commitments = Set.insert e.commitment state.commitments
                , process_commitments = Set.insert { process = e.process, commitment = e.commitment } state.process_commitments
                , lastEventTime = e.posixtime
            }

        EventTypeAdded e ->
            { state
                | eventTypes = Set.insert e.eventType state.eventTypes
                , lastEventTime = e.posixtime
            }

        LinkedEventTypeToProcessType e ->
            let
                ptet =
                    { etype = e.etype, ptype = e.ptype }
            in
            { state
                | processType_eventTypes = Set.insert ptet state.processType_eventTypes
                , lastEventTime = e.posixtime
            }

        EventTypeRemoved e ->
            { state
                | eventTypes = Set.remove e.eventType state.eventTypes
                , lastEventTime = e.posixtime
            }

        EventAdded e ->
            { state
                | events = Set.insert e.event state.events
                , process_events = Set.insert { process = e.process, event = e.event } state.process_events
                , lastEventTime = e.posixtime
            }

        ConnectionInitiated e ->
            { state
                | sessionUuid = Just e.sessionUuid
                , lastEventTime = e.posixtime
            }



-- Event constructors


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


processAdded : Uuid -> Time.Posix -> String -> Event
processAdded uuid posixtime ptype =
    ProcessAdded
        { uuid = uuid
        , posixtime = posixtime
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
                        Decode.map3 processAdded
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
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

                    _ ->
                        Decode.fail "Unknown Event type"
            )


currentProcessType : State -> Maybe ProcessType
currentProcessType state =
    case state.route of
        Route.Processes maybetype ->
            getProcessType state (Maybe.withDefault "" maybetype)

        _ ->
            Nothing


getProcessType : State -> String -> Maybe ProcessType
getProcessType state type_str =
    Set.filter
        (\pt -> pt.name == type_str)
        state.processTypes
        |> Set.values
        |> List.head
