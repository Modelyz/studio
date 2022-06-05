module State exposing (State, aggregate, empty, getCommitmentTypes, getCommitments, getEntityType, getEventTypes, getEvents, getProcess, getProcessType)

--import REA.ProcessType as PT exposing (ProcessType)

import Browser.Navigation as Nav
import DictSet as Set exposing (DictSet)
import Event exposing (Event(..), EventPayload(..), base)
import EventFlow exposing (EventFlow(..))
import IOStatus exposing (IOStatus(..))
import Json.Decode exposing (andThen)
import Prng.Uuid as Uuid exposing (Uuid)
import REA.Agent as A exposing (Agent)
import REA.AgentType as AT exposing (AgentType)
import REA.Commitment as CM exposing (Commitment)
import REA.CommitmentType as CMT exposing (CommitmentType)
import REA.Contract as CN exposing (Contract)
import REA.ContractType as CNT exposing (ContractType)
import REA.Entity as EN exposing (Entity)
import REA.EntityType as ENT exposing (EntityType)
import REA.Event as E
import REA.EventType as ET exposing (EventType)
import REA.Group as G exposing (Group, compare)
import REA.Ident as Ident exposing (EntityIdentifier, Identifier, IdentifierType)
import REA.Process as P exposing (Process)
import REA.ProcessCommitments as PC exposing (ProcessCommitments)
import REA.ProcessEvents as PE exposing (ProcessEvents)
import REA.ProcessType as PT exposing (ProcessType)
import REA.ProcessTypeCommitmentType as PTCT exposing (ProcessTypeCommitmentType)
import REA.ProcessTypeEventType as PTET exposing (ProcessTypeEventType)
import REA.Resource as R exposing (Resource)
import REA.ResourceType as RT exposing (ResourceType)
import Random.Pcg.Extended as Random exposing (Seed, initialSeed)
import Result exposing (Result(..))
import Time exposing (millisToPosix)
import Websocket exposing (WSStatus(..))


type alias State =
    { pendingEvents : DictSet Int Event
    , lastEventTime : Time.Posix
    , uuids : DictSet String Uuid

    -- entity types
    , entityTypes : DictSet String EntityType -- TODO remove others below
    , resourceTypes : DictSet String ResourceType
    , eventTypes : DictSet String EventType
    , agentTypes : DictSet String AgentType
    , commitmentTypes : DictSet String CommitmentType
    , contractTypes : DictSet String ContractType
    , processTypes : DictSet String ProcessType

    -- entities
    , entities : DictSet String Entity
    , resources : DictSet String Resource
    , events : DictSet Int E.Event
    , agents : DictSet String Agent
    , commitments : DictSet Int Commitment
    , contracts : DictSet String Contract
    , processes : DictSet Int Process

    -- links
    , process_commitments : DictSet String ProcessCommitments
    , process_events : DictSet String ProcessEvents
    , processType_commitmentTypes : DictSet String ProcessTypeCommitmentType
    , processType_eventTypes : DictSet String ProcessTypeEventType

    -- behaviours
    , groups : DictSet String Group
    , identifierTypes : DictSet String IdentifierType
    , identifiers : DictSet String EntityIdentifier
    }


empty : State
empty =
    { pendingEvents = Set.empty Event.compare
    , lastEventTime = millisToPosix 0
    , uuids = Set.empty Uuid.toString

    -- entity types
    , entityTypes = Set.empty ENT.compare
    , resourceTypes = Set.empty RT.compare
    , eventTypes = Set.empty ET.compare
    , agentTypes = Set.empty AT.compare
    , commitmentTypes = Set.empty CMT.compare
    , contractTypes = Set.empty CNT.compare
    , processTypes = Set.empty PT.compare

    -- entities
    , entities = Set.empty EN.compare
    , resources = Set.empty R.compare
    , events = Set.empty E.compare
    , agents = Set.empty A.compare
    , commitments = Set.empty CM.compare
    , contracts = Set.empty CN.compare
    , processes = Set.empty P.compare

    -- links
    , process_events = Set.empty PE.compare
    , processType_commitmentTypes = Set.empty PTCT.compare
    , process_commitments = Set.empty PC.compare
    , processType_eventTypes = Set.empty PTET.compare
    , groups = Set.empty G.compare

    -- behaviours
    , identifierTypes = Set.empty Ident.compareIdentifierType
    , identifiers = Set.empty Ident.compareEntityIdentifier
    }


aggregate : Event -> State -> State
aggregate (Event b p) state =
    case p of
        ProcessTypeChanged e ->
            { state
                | processTypes = Set.insert e state.processTypes
                , lastEventTime = b.when
                , pendingEvents = updatePending (Event b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        ProcessTypeRemoved e ->
            { state
                | processTypes = Set.filter (\pt -> pt.name /= e) state.processTypes
                , lastEventTime = b.when
                , pendingEvents = updatePending (Event b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        ProcessAdded e ->
            { state
                | processes = Set.insert e state.processes
                , lastEventTime = b.when
                , pendingEvents = updatePending (Event b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        CommitmentTypeAdded e ->
            { state
                | commitmentTypes = Set.insert e state.commitmentTypes
                , lastEventTime = b.when
                , pendingEvents = updatePending (Event b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        CommitmentTypeRemoved e ->
            { state
                | commitmentTypes = Set.filter (\ct -> ct.name /= e) state.commitmentTypes
                , lastEventTime = b.when
                , pendingEvents = updatePending (Event b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        CommitmentAdded e ->
            { state
                | commitments = Set.insert e state.commitments
                , lastEventTime = b.when
                , pendingEvents = updatePending (Event b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        LinkedEventTypeToProcessType e ->
            let
                ptet =
                    { etype = e.etype, ptype = e.ptype }
            in
            { state
                | processType_eventTypes = Set.insert ptet state.processType_eventTypes
                , lastEventTime = b.when
                , pendingEvents = updatePending (Event b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        LinkedCommitmentTypeToProcessType e ->
            let
                ptct =
                    { ctype = e.ctype, ptype = e.ptype }
            in
            { state
                | processType_commitmentTypes = Set.insert ptct state.processType_commitmentTypes
                , lastEventTime = b.when
                , pendingEvents = updatePending (Event b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        EventAdded e ->
            -- TODO : link the Event to a Process (cf process_events)
            { state
                | events = Set.insert e state.events
                , lastEventTime = b.when
                , pendingEvents = updatePending (Event b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        ConnectionInitiated e ->
            { state
                | lastEventTime = b.when
                , pendingEvents = updatePending (Event b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        GroupAdded e ->
            { state
                | groups = Set.insert (Group e.name) state.groups
                , lastEventTime = b.when
                , pendingEvents = updatePending (Event b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        GroupRemoved e ->
            { state
                | groups = Set.filter (\g -> g.name /= e) state.groups
                , lastEventTime = b.when
                , pendingEvents = updatePending (Event b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        IdentifierTypeAdded e ->
            { state
                | identifierTypes = Set.insert e state.identifierTypes
                , lastEventTime = b.when
                , pendingEvents = updatePending (Event b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        IdentifierTypeRemoved e ->
            { state
                | identifierTypes = Set.filter (\i -> i.name /= e) state.identifierTypes
                , lastEventTime = b.when
                , pendingEvents = updatePending (Event b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        TypeAdded e ->
            { state
                | entityTypes = Set.insert e state.entityTypes
                , lastEventTime = b.when
                , pendingEvents = updatePending (Event b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        Added e ->
            { state
                | entities = Set.insert e state.entities
                , lastEventTime = b.when
                , pendingEvents = updatePending (Event b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        Removed e ->
            { state
                | entities = Set.remove e state.entities
                , lastEventTime = b.when
                , pendingEvents = updatePending (Event b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        TypeRemoved et ->
            { state
                | entityTypes = Set.remove et state.entityTypes
                , lastEventTime = b.when
                , pendingEvents = updatePending (Event b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        IdentifierAdded e ->
            { state
                | identifiers = Set.insert e state.identifiers
                , lastEventTime = b.when
                , pendingEvents = updatePending (Event b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
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


getEntityType : State -> String -> Maybe EntityType
getEntityType state name =
    Set.filter (\et -> ENT.toName et == name) state.entityTypes
        |> Set.toList
        |> List.head


getEventTypes : State -> String -> DictSet String EventType
getEventTypes state name =
    let
        etnames =
            Set.filter (\ptet -> ptet.ptype == name) state.processType_eventTypes
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
        cs =
            Set.filter (\pc -> pc.process == process.uuid) state.process_commitments
                |> Set.map Uuid.toString (\pc -> pc.commitment)
    in
    Set.filter (\c -> Set.member c.uuid cs) state.commitments


getEvents : State -> Process -> DictSet Int E.Event
getEvents state process =
    let
        es =
            Set.filter (\pe -> pe.process == process.uuid) state.process_events
                |> Set.map Uuid.toString (\pe -> pe.event)
    in
    Set.filter (\e -> Set.member e.uuid es) state.events


getProcessType : State -> String -> Maybe ProcessType
getProcessType state type_str =
    Set.filter
        (\pt -> pt.name == type_str)
        state.processTypes
        |> Set.values
        |> List.head
