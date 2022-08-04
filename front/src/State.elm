module State exposing (State, aggregate, empty)

import Configuration exposing (Configuration)
import DictSet as Set exposing (DictSet)
import Entity.Entity as Entity exposing (Entity)
import Group.Group as Group exposing (Group)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType as IdentifierType exposing (IdentifierType)
import Link.Link as Link exposing (Link(..))
import Message exposing (Message(..), Payload(..), base)
import MessageFlow exposing (MessageFlow(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Relation.ProcessCommitments as PC exposing (ProcessCommitments)
import Relation.ProcessEvents as PE exposing (ProcessEvents)
import Restriction.Restriction as Restriction exposing (Restriction)
import Time exposing (millisToPosix)


type alias State =
    { pendingMessages : DictSet Int Message
    , lastMessageTime : Time.Posix
    , uuids : DictSet String Uuid

    -- entities
    , entities : DictSet String Entity
    , groups : DictSet String Group

    -- links
    , process_commitments : DictSet String ProcessCommitments
    , process_events : DictSet String ProcessEvents
    , restrictions : DictSet String Restriction
    , grouped : DictSet String Link

    -- ident
    , identifierTypes : DictSet String IdentifierType
    , identifiers : DictSet String Identifier

    -- config
    , configs : DictSet String Configuration
    }


empty : State
empty =
    { pendingMessages = Set.empty Message.compare
    , lastMessageTime = millisToPosix 0
    , uuids = Set.empty Uuid.toString

    -- entities
    , entities = Set.empty Entity.compare
    , groups = Set.empty Group.compare

    -- links
    , process_events = Set.empty PE.compare
    , restrictions = Set.empty Restriction.compare
    , process_commitments = Set.empty PC.compare
    , grouped = Set.empty Link.compare

    -- behaviours
    , identifierTypes = Set.empty IdentifierType.compare
    , identifiers = Set.empty Identifier.compare

    -- config
    , configs = Set.empty Configuration.compare
    }


aggregate : Message -> State -> State
aggregate (Message b p) state =
    case p of
        Restricted r ->
            { state
                | restrictions = Set.insert r state.restrictions
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        -- TODO : link the Message to a Process (cf process_events)
        ConnectionInitiated e ->
            { state
                | lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        IdentifierTypeAdded e ->
            { state
                | identifierTypes = Set.insert e state.identifierTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        IdentifierTypeRemoved e ->
            { state
                | identifierTypes = Set.filter (\i -> i /= e) state.identifierTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        Added e ->
            { state
                | entities = Set.insert e state.entities
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        Removed e ->
            { state
                | entities = Set.remove e state.entities
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        IdentifierAdded ei ->
            { state
                | identifiers = Set.insert ei state.identifiers
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        Configured conf ->
            { state
                | configs = Set.insert conf state.configs
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        Unconfigured conf ->
            { state
                | configs = Set.remove conf state.configs
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        DefinedGroup group ->
            { state
                | groups = Set.insert group state.groups
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        RemovedGroup group ->
            { state
                | groups = Set.remove group state.groups
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        Grouped e g ->
            { state
                | grouped = Set.insert (EntityGroup e g) state.grouped
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        Ungrouped e g ->
            { state
                | grouped = Set.remove (EntityGroup e g) state.grouped
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }


updatePending : Message -> DictSet Int Message -> DictSet Int Message
updatePending e es =
    case .flow <| base <| e of
        Requested ->
            Set.insert e es

        Processed ->
            Set.remove e es
