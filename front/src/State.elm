module State exposing (State, aggregate, currentProcessType, getCommitmentTypes, getCommitments, getEventTypes, getEvents, getProcess, getProcessType, new)

--import REA.ProcessType as PT exposing (ProcessType)

import Browser.Navigation as Nav
import DictSet as Set exposing (DictSet)
import Event exposing (Event(..), base)
import EventFlow exposing (EventFlow(..))
import IOStatus exposing (IOStatus(..))
import Json.Decode exposing (andThen)
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
import Time exposing (millisToPosix)
import Websocket exposing (WSStatus(..))



-- store events to IDB then send to WS
-- global application state --
-- TODO : différencier le state et le model ? le state est l'agrégation des evenements, le model est ce qui représente l'ui. (alors le state serait dans le model)
-- → switch to elm-spa


type alias State =
    -- ui model related
    { currentSeed : Seed
    , navkey : Nav.Key
    , route : Route

    -- input related
    , inputProcessType : ProcessType
    , inputCommitmentType : String
    , inputCommitmentTypeProcessTypes : DictSet String String
    , inputEventType : String
    , inputEventTypeProcessTypes : DictSet String String

    -- ES and WS related
    , iostatus : IOStatus
    , lastEventTime : Time.Posix
    , pendingEvents : DictSet Int Event
    , uuids : DictSet String Uuid

    -- WS related
    , wsstatus : WSStatus
    , timeoutReconnect : Int

    -- session related
    , sessionUuid : Maybe Uuid

    -- REA state related
    , processTypes : DictSet String ProcessType
    , processes : DictSet Int Process
    , commitmentTypes : DictSet String CommitmentType
    , commitments : DictSet Int Commitment
    , process_commitments : DictSet String ProcessCommitments
    , eventTypes : DictSet String EventType
    , events : DictSet Int E.Event
    , process_events : DictSet String ProcessEvents
    , processType_commitmentTypes : DictSet String ProcessTypeCommitmentType
    , processType_eventTypes : DictSet String ProcessTypeEventType
    }


new : Seed -> Nav.Key -> Route -> State
new seed key route =
    { currentSeed = seed
    , navkey = key
    , route = route
    , iostatus = ESReading
    , wsstatus = WSClosed
    , timeoutReconnect = 1
    , inputProcessType = ProcessType ""
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
    , pendingEvents = Set.empty Event.compare
    , uuids = Set.empty Uuid.toString
    , sessionUuid = Nothing
    , process_events = Set.empty PE.compare
    , processType_commitmentTypes = Set.empty PTCT.compare
    , processType_eventTypes = Set.empty PTET.compare
    }


aggregate : Event -> State -> State
aggregate event state =
    case event of
        ProcessTypeChanged e ->
            { state
                | processTypes = Set.insert e.ptype state.processTypes
                , lastEventTime = e.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        ProcessTypeRemoved e ->
            { state
                | processTypes = Set.filter (\pt -> pt.name /= e.ptype) state.processTypes
                , lastEventTime = e.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        ProcessAdded e ->
            let
                p =
                    { uuid = e.uuid, posixtime = e.posixtime, name = e.name, type_ = e.type_ }
            in
            { state
                | processes = Set.insert p state.processes
                , lastEventTime = e.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        CommitmentTypeAdded e ->
            { state
                | commitmentTypes = Set.insert e.commitmentType state.commitmentTypes
                , lastEventTime = e.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        CommitmentTypeRemoved e ->
            { state
                | commitmentTypes = Set.remove e.commitmentType state.commitmentTypes
                , lastEventTime = e.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        CommitmentAdded e ->
            { state
                | commitments = Set.insert e.commitment state.commitments
                , process_commitments = Set.insert { process = e.process.name, commitment = e.commitment.name } state.process_commitments
                , lastEventTime = e.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        EventTypeAdded e ->
            { state
                | eventTypes = Set.insert e.eventType state.eventTypes
                , lastEventTime = e.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        LinkedEventTypeToProcessType e ->
            let
                ptet =
                    { etype = e.etype, ptype = e.ptype }
            in
            { state
                | processType_eventTypes = Set.insert ptet state.processType_eventTypes
                , lastEventTime = e.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        EventTypeRemoved e ->
            { state
                | eventTypes = Set.remove e.eventType state.eventTypes
                , lastEventTime = e.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        EventAdded e ->
            { state
                | events = Set.insert e.event state.events
                , process_events = Set.insert { process = e.process.name, event = e.event.name } state.process_events
                , lastEventTime = e.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        ConnectionInitiated e ->
            { state
                | sessionUuid = Just e.sessionUuid
                , lastEventTime = e.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }


updatePending : Event -> DictSet Int Event -> DictSet Int Event
updatePending e es =
    case .flow <| base <| e of
        Requested ->
            Set.insert e es

        Processed ->
            Set.remove e es


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


getEventTypes : State -> String -> DictSet String EventType
getEventTypes state etype =
    let
        etnames =
            Set.filter (\ptet -> ptet.ptype == etype) state.processType_eventTypes
                |> Set.map identity (\ptet -> ptet.etype)
    in
    Set.filter (\et -> Set.member et.name etnames) state.eventTypes


getCommitmentTypes : State -> String -> DictSet String CommitmentType
getCommitmentTypes state ptype =
    let
        ctnames =
            Set.filter (\ptct -> ptct.ptype == ptype) state.processType_commitmentTypes
                |> Set.map identity (\ptct -> ptct.ctype)
    in
    Set.filter (\ct -> Set.member ct.name ctnames) state.commitmentTypes


getCommitments : State -> Process -> DictSet Int Commitment
getCommitments state process =
    let
        cnames =
            Set.filter (\pc -> pc.process == process.name) state.process_commitments
                |> Set.map identity (\pc -> pc.commitment)
    in
    Set.filter (\c -> Set.member c.name cnames) state.commitments


getEvents : State -> Process -> DictSet Int E.Event
getEvents state process =
    let
        enames =
            Set.filter (\pe -> pe.process == process.name) state.process_events
                |> Set.map identity (\pe -> pe.event)
    in
    Set.filter (\e -> Set.member e.name enames) state.events


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
