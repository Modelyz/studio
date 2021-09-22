port module ES exposing (Event(..), State, aggregate, compare, currentProcessType, decoder, encode, getCommitments, getEvents, getEventstore, getProcess, getProcessType, new, storeEvent)

import Browser.Navigation as Nav
import DictSet as Set exposing (DictSet)
import Json.Decode as Decode exposing (Decoder, andThen)
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
import Status exposing (Status(..))
import Time exposing (millisToPosix, posixToMillis)


port getEventstore : Encode.Value -> Cmd msg


port storeEvent : Encode.Value -> Cmd msg



-- application/user events --


type Event
    = ProcessTypeChanged
        { uuid : Uuid.Uuid
        , posixtime : Time.Posix
        , processType : ProcessType
        }
    | ProcessTypeRemoved
        { uuid : Uuid.Uuid
        , posixtime : Time.Posix
        , processType : ProcessType
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



--    | LinkedEventTypeToProcessType
--        { uuid : Uuid.Uuid
--        , posixtime : Time.Posix
--        , etype : String
--        , ptype : String
--        }
-- global application state --
-- TODO : différencier le state et le model ? le state est l'agrégation des evenements, le model est ce qui représente l'ui. (alors le state serait dans le model)


type alias State =
    -- model related
    { currentSeed : Seed
    , navkey : Nav.Key
    , route : Route
    , status : Status

    -- input related
    , inputProcessType : ProcessType
    , inputCommitmentType : String
    , inputCommitmentTypeProcessTypes : DictSet String String
    , inputEventType : String
    , inputEventTypeProcessTypes : DictSet String String

    -- state related
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
    , status = Loading
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
                | processTypes = Set.insert e.processType state.processTypes
            }

        ProcessTypeRemoved e ->
            { state
                | processTypes = Set.remove e.processType state.processTypes
            }

        ProcessAdded e ->
            { state | processes = Set.insert e state.processes }

        CommitmentTypeAdded e ->
            { state
                | commitmentTypes = Set.insert e.commitmentType state.commitmentTypes
            }

        CommitmentTypeRemoved e ->
            { state
                | commitmentTypes = Set.remove e.commitmentType state.commitmentTypes
            }

        CommitmentAdded e ->
            { state
                | commitments = Set.insert e.commitment state.commitments
                , process_commitments = Set.insert { process = e.process, commitment = e.commitment } state.process_commitments
            }

        EventTypeAdded e ->
            { state
                | eventTypes = Set.insert e.eventType state.eventTypes
            }

        EventTypeRemoved e ->
            { state
                | eventTypes = Set.remove e.eventType state.eventTypes
            }

        EventAdded e ->
            { state
                | events = Set.insert e.event state.events
                , process_events = Set.insert { process = e.process, event = e.event } state.process_events
            }



-- Event constructors


processTypeChanged : Uuid -> Time.Posix -> ProcessType -> Event
processTypeChanged uuid posixtime ptype =
    ProcessTypeChanged
        { uuid = uuid
        , posixtime = posixtime
        , processType = ptype
        }


processTypeRemoved : Uuid -> Time.Posix -> ProcessType -> Event
processTypeRemoved uuid posixtime processType =
    ProcessTypeRemoved
        { uuid = uuid
        , posixtime = posixtime
        , processType = processType
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


encode : Event -> Encode.Value
encode event =
    case event of
        ProcessTypeChanged e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "type", Encode.string "ProcessTypeChanged" )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "processType", PT.encode e.processType )
                ]

        ProcessTypeRemoved e ->
            Encode.object
                [ ( "uuid", Uuid.encode e.uuid )
                , ( "type", Encode.string "ProcessTypeRemoved" )
                , ( "posixtime", Encode.int <| posixToMillis e.posixtime )
                , ( "processType", PT.encode e.processType )
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
                            (Decode.field "processType" PT.decoder)

                    "ProcessTypeRemoved" ->
                        Decode.map3 processTypeRemoved
                            (Decode.field "uuid" Uuid.decoder)
                            (Decode.field "posixtime" Decode.int |> andThen toPosix)
                            (Decode.field "processType" PT.decoder)

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
