module State exposing (State, aggregate, empty, getCommitmentTypes, getCommitments, getEventTypes, getEvents, getProcess, getProcessType)

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
import REA.Group as G exposing (Group, compare)
import REA.Process as P exposing (Process)
import REA.ProcessCommitments as PC exposing (ProcessCommitments)
import REA.ProcessEvents as PE exposing (ProcessEvents)
import REA.ProcessType as PT exposing (ProcessType)
import REA.ProcessTypeCommitmentType as PTCT exposing (ProcessTypeCommitmentType)
import REA.ProcessTypeEventType as PTET exposing (ProcessTypeEventType)
import Random.Pcg.Extended as Random exposing (Seed, initialSeed)
import Result exposing (Result(..))
import Time exposing (millisToPosix)
import Websocket exposing (WSStatus(..))


type alias State =
    { pendingEvents : DictSet Int Event
    , lastEventTime : Time.Posix
    , uuids : DictSet String Uuid
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
    , groups : DictSet String Group
    }


empty : State
empty =
    { pendingEvents = Set.empty Event.compare
    , lastEventTime = millisToPosix 0
    , uuids = Set.empty Uuid.toString
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
    , groups = Set.empty G.compare
    }


aggregate : Event -> State -> State
aggregate event state =
    case event of
        ProcessTypeChanged e b ->
            { state
                | processTypes = Set.insert e.ptype state.processTypes
                , lastEventTime = b.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        ProcessTypeRemoved e b ->
            { state
                | processTypes = Set.filter (\pt -> pt.name /= e.ptype) state.processTypes
                , lastEventTime = b.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        ProcessAdded e b ->
            let
                p =
                    { uuid = b.uuid, posixtime = b.posixtime, name = e.name, type_ = e.type_ }
            in
            { state
                | processes = Set.insert p state.processes
                , lastEventTime = b.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        CommitmentTypeAdded e b ->
            { state
                | commitmentTypes = Set.insert e.commitmentType state.commitmentTypes
                , lastEventTime = b.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        CommitmentTypeRemoved e b ->
            { state
                | commitmentTypes = Set.remove e.commitmentType state.commitmentTypes
                , lastEventTime = b.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        CommitmentAdded e b ->
            { state
                | commitments = Set.insert e.commitment state.commitments
                , process_commitments = Set.insert { process = e.process.name, commitment = e.commitment.name } state.process_commitments
                , lastEventTime = b.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        EventTypeAdded e b ->
            { state
                | eventTypes = Set.insert e.eventType state.eventTypes
                , lastEventTime = b.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        LinkedEventTypeToProcessType e b ->
            let
                ptet =
                    { etype = e.etype, ptype = e.ptype }
            in
            { state
                | processType_eventTypes = Set.insert ptet state.processType_eventTypes
                , lastEventTime = b.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        EventTypeRemoved e b ->
            { state
                | eventTypes = Set.remove e.eventType state.eventTypes
                , lastEventTime = b.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        EventAdded e b ->
            { state
                | events = Set.insert e.event state.events
                , process_events = Set.insert { process = e.process.name, event = e.event.name } state.process_events
                , lastEventTime = b.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        ConnectionInitiated e b ->
            { state
                | lastEventTime = b.posixtime
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        GroupAdded e b ->
            { state
                | groups = Set.insert (Group e.name e.entity) state.groups
                , pendingEvents = updatePending event state.pendingEvents
                , uuids = Set.insert (.uuid <| base event) state.uuids
            }

        GroupRemoved e b ->
            { state
                | groups = Set.remove (Group e.name e.entity) state.groups
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


getProcessType : State -> String -> Maybe ProcessType
getProcessType state type_str =
    Set.filter
        (\pt -> pt.name == type_str)
        state.processTypes
        |> Set.values
        |> List.head
