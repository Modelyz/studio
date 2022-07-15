module State exposing (State, aggregate, empty)

import Dict exposing (Dict)
import DictSet as Set exposing (DictSet)
import Entity.Entity as EN exposing (Entity(..))
import EntityType.EntityType as ENT exposing (EntityType)
import EventFlow exposing (EventFlow(..))
import Events exposing (EventPayload(..), Message(..), base)
import Ident.EntityIdentifier as EntityIdentifier exposing (EntityIdentifier)
import Ident.IdentifierType as IdentifierType exposing (IdentifierType)
import Prng.Uuid as Uuid exposing (Uuid)
import Relation.ProcessCommitments as PC exposing (ProcessCommitments)
import Relation.ProcessEvents as PE exposing (ProcessEvents)
import Restriction.Restriction as Restriction exposing (Restriction)
import Time exposing (millisToPosix)


type alias State =
    { pendingEvents : DictSet Int Message
    , lastEventTime : Time.Posix
    , uuids : DictSet String Uuid

    -- entity types
    , entityTypes : DictSet String EntityType
    , processTypes : DictSet String EntityType

    -- entities
    , entities : DictSet String Entity -- TODO gather all in entities

    -- links
    , process_commitments : DictSet String ProcessCommitments
    , process_events : DictSet String ProcessEvents
    , restrictions : DictSet String Restriction

    -- behaviours
    , identifierTypes : DictSet String IdentifierType
    , identifiers : DictSet String EntityIdentifier
    }


empty : State
empty =
    { pendingEvents = Set.empty Events.compare
    , lastEventTime = millisToPosix 0
    , uuids = Set.empty Uuid.toString

    -- entity types
    , entityTypes = Set.empty ENT.compare
    , processTypes = Set.empty ENT.compare

    -- entities
    , entities = Set.empty EN.compare

    -- links
    , process_events = Set.empty PE.compare
    , restrictions = Set.empty Restriction.compare
    , process_commitments = Set.empty PC.compare

    -- behaviours
    , identifierTypes = Set.empty IdentifierType.compare
    , identifiers = Set.empty EntityIdentifier.compare
    }


aggregate : Message -> State -> State
aggregate (Message b p) state =
    case p of
        ProcessTypeChanged e ->
            { state
                | processTypes = Set.insert e state.processTypes
                , lastEventTime = b.when
                , pendingEvents = updatePending (Message b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        ProcessTypeRemoved e ->
            { state
                | processTypes = Set.filter (\pt -> ENT.toName pt /= e) state.processTypes
                , lastEventTime = b.when
                , pendingEvents = updatePending (Message b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        Restricted r ->
            { state
                | restrictions = Set.insert r state.restrictions
                , lastEventTime = b.when
                , pendingEvents = updatePending (Message b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        -- TODO : link the Event to a Process (cf process_events)
        ConnectionInitiated e ->
            { state
                | lastEventTime = b.when
                , pendingEvents = updatePending (Message b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        IdentifierTypeAdded e ->
            { state
                | identifierTypes = Set.insert e state.identifierTypes
                , lastEventTime = b.when
                , pendingEvents = updatePending (Message b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        IdentifierTypeRemoved e ->
            { state
                | identifierTypes = Set.filter (\i -> i /= e) state.identifierTypes
                , lastEventTime = b.when
                , pendingEvents = updatePending (Message b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        TypeAdded e ->
            { state
                | entityTypes = Set.insert e state.entityTypes
                , lastEventTime = b.when
                , pendingEvents = updatePending (Message b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        Added e ->
            { state
                | entities = Set.insert e state.entities
                , lastEventTime = b.when
                , pendingEvents = updatePending (Message b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        Removed e ->
            { state
                | entities = Set.remove e state.entities
                , lastEventTime = b.when
                , pendingEvents = updatePending (Message b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        TypeRemoved et ->
            { state
                | entityTypes = Set.remove et state.entityTypes
                , lastEventTime = b.when
                , pendingEvents = updatePending (Message b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }

        IdentifierAdded ei ->
            { state
                | identifiers = Set.insert ei state.identifiers
                , lastEventTime = b.when
                , pendingEvents = updatePending (Message b p) state.pendingEvents
                , uuids = Set.insert b.uuid state.uuids
            }


updatePending : Message -> DictSet Int Message -> DictSet Int Message
updatePending e es =
    case .flow <| base <| e of
        Requested ->
            Set.insert e es

        Processed ->
            Set.remove e es
