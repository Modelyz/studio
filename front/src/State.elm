module State exposing (State, aggregate, empty, getCommitments, getEvents, getProcess)

import Dict exposing (Dict)
import DictSet as Set exposing (DictSet)
import Event exposing (Event(..), EventPayload(..), base)
import EventFlow exposing (EventFlow(..))
import Group.Group as Group exposing (Group, compare)
import Ident.EntityIdentifier as EntityIdentifier exposing (EntityIdentifier)
import Ident.IdentifierType as IdentifierType exposing (IdentifierType)
import Prng.Uuid as Uuid exposing (Uuid)
import REA.Agent as A exposing (Agent)
import REA.Commitment as CM exposing (Commitment)
import REA.Contract as CN exposing (Contract)
import REA.Entity as EN exposing (Entity)
import REA.EntityType as ENT exposing (EntityType)
import REA.Event as E
import REA.Process as P exposing (Process)
import REA.ProcessCommitments as PC exposing (ProcessCommitments)
import REA.ProcessEvents as PE exposing (ProcessEvents)
import REA.Resource as R exposing (Resource)
import REA.Restriction as Restriction exposing (Restriction)
import Time exposing (millisToPosix)


type alias State =
    { pendingEvents : DictSet Int Event
    , lastEventTime : Time.Posix
    , uuids : DictSet String Uuid

    -- entity types
    , entityTypes : DictSet String EntityType
    , processTypes : DictSet String EntityType

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
    , restrictions : DictSet String Restriction

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
    , processTypes = Set.empty ENT.compare

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
    , restrictions = Set.empty Restriction.compare
    , process_commitments = Set.empty PC.compare
    , groups = Set.empty Group.compare

    -- behaviours
    , identifierTypes = Set.empty IdentifierType.compare
    , identifiers = Set.empty EntityIdentifier.compare
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
                | processTypes = Set.filter (\pt -> ENT.toName pt /= e) state.processTypes
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

        CommitmentAdded e ->
            { state
                | commitments = Set.insert e state.commitments
                , lastEventTime = b.when
                , pendingEvents = updatePending (Event b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        Restricted r ->
            { state
                | restrictions = Set.insert r state.restrictions
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
                | identifierTypes = Set.filter (\i -> i /= e) state.identifierTypes
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

        IdentifierAdded ei ->
            { state
                | identifiers = Set.insert ei state.identifiers
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
