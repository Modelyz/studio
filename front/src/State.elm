module State exposing (State, aggregate, allHierarchic, allTyped, empty)

import Agent.Agent as Agent exposing (Agent)
import AgentType.AgentType as AgentType exposing (AgentType)
import Commitment.Commitment as Commitment exposing (Commitment)
import CommitmentType.CommitmentType as CommitmentType exposing (CommitmentType)
import Configuration exposing (Configuration)
import Contract.Contract as Contract exposing (Contract)
import ContractType.ContractType as ContractType exposing (ContractType)
import DictSet as Set exposing (DictSet)
import Entity.Entity as Entity exposing (Entity)
import Event.Event as Event exposing (Event)
import EventType.EventType as EventType exposing (EventType)
import Group.Group as Group exposing (Group)
import Group.Link as GroupLink
import GroupType.GroupType as GroupType exposing (GroupType)
import Hierarchy.Hierarchic as Hierarchic exposing (Hierarchic)
import Hierarchy.Type as HType
import Ident.Identifiable as Identifiable exposing (Identifiable)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType as IdentifierType exposing (IdentifierType)
import Item.Item as Item exposing (Item, OnlyItem)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Message exposing (Message(..), Payload(..), base)
import MessageFlow exposing (MessageFlow(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Process as Process exposing (Process)
import ProcessType.ProcessType as ProcessType exposing (ProcessType)
import Relation.ProcessCommitments as PC exposing (ProcessCommitments)
import Relation.ProcessEvents as PE exposing (ProcessEvents)
import Resource.Resource as Resource exposing (Resource)
import ResourceType.ResourceType as ResourceType exposing (ResourceType)
import Restriction.Restriction as Restriction exposing (Restriction)
import Time exposing (millisToPosix)
import Type exposing (Type(..))
import Typed.Type as TType
import Typed.Typed as Typed exposing (Typed)


type alias State =
    { pendingMessages : DictSet Int Message
    , lastMessageTime : Time.Posix
    , uuids : DictSet String Uuid

    -- entities
    --, entities : DictSet String Entity
    , resources : DictSet String Resource
    , events : DictSet String Event
    , agents : DictSet String Agent
    , commitments : DictSet String Commitment
    , resourceTypes : DictSet String ResourceType
    , eventTypes : DictSet String EventType
    , agentTypes : DictSet String AgentType
    , commitmentTypes : DictSet String CommitmentType
    , contracts : DictSet String Contract
    , contractTypes : DictSet String ContractType
    , processes : DictSet String Process
    , processTypes : DictSet String ProcessType
    , groups : DictSet String Group
    , groupTypes : DictSet String GroupType

    -- links
    , process_commitments : DictSet String ProcessCommitments
    , process_events : DictSet String ProcessEvents
    , restrictions : DictSet String Restriction
    , grouped : DictSet String GroupLink.Link

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
    --, entities = Set.empty Entity.compare
    , resources = Set.empty Resource.compare
    , events = Set.empty Event.compare
    , agents = Set.empty Agent.compare
    , commitments = Set.empty Commitment.compare
    , resourceTypes = Set.empty ResourceType.compare
    , eventTypes = Set.empty EventType.compare
    , agentTypes = Set.empty AgentType.compare
    , commitmentTypes = Set.empty CommitmentType.compare
    , contracts = Set.empty Contract.compare
    , contractTypes = Set.empty ContractType.compare
    , processes = Set.empty Process.compare
    , processTypes = Set.empty ProcessType.compare
    , groups = Set.empty Group.compare
    , groupTypes = Set.empty GroupType.compare

    -- links
    , process_events = Set.empty PE.compare
    , restrictions = Set.empty Restriction.compare
    , process_commitments = Set.empty PC.compare
    , grouped = Set.empty GroupLink.compare

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

        AddedResourceType rt ->
            { state
                | resourceTypes = Set.insert rt state.resourceTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        RemovedResourceType rt ->
            { state
                | resourceTypes = Set.remove rt state.resourceTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        AddedEventType et ->
            { state
                | eventTypes = Set.insert et state.eventTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        RemovedEventType et ->
            { state
                | eventTypes = Set.remove et state.eventTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        AddedAgentType at ->
            { state
                | agentTypes = Set.insert at state.agentTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        RemovedAgentType at ->
            { state
                | agentTypes = Set.remove at state.agentTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        AddedCommitmentType cmt ->
            { state
                | commitmentTypes = Set.insert cmt state.commitmentTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        RemovedCommitmentType cmt ->
            { state
                | commitmentTypes = Set.remove cmt state.commitmentTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        AddedContractType cnt ->
            { state
                | contractTypes = Set.insert cnt state.contractTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        RemovedContractType cnt ->
            { state
                | contractTypes = Set.remove cnt state.contractTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        AddedProcessType pt ->
            { state
                | processTypes = Set.insert pt state.processTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        RemovedProcessType pt ->
            { state
                | processTypes = Set.remove pt state.processTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        AddedResource rt ->
            { state
                | resources = Set.insert rt state.resources
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        RemovedResource rt ->
            { state
                | resources = Set.remove rt state.resources
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        AddedEvent et ->
            { state
                | events = Set.insert et state.events
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        RemovedEvent et ->
            { state
                | events = Set.remove et state.events
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        AddedAgent at ->
            { state
                | agents = Set.insert at state.agents
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        RemovedAgent at ->
            { state
                | agents = Set.remove at state.agents
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        AddedCommitment cmt ->
            { state
                | commitments = Set.insert cmt state.commitments
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        RemovedCommitment cmt ->
            { state
                | commitments = Set.remove cmt state.commitments
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        AddedContract cnt ->
            { state
                | contracts = Set.insert cnt state.contracts
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        RemovedContract cnt ->
            { state
                | contracts = Set.remove cnt state.contracts
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        AddedProcess pt ->
            { state
                | processes = Set.insert pt state.processes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        RemovedProcess pt ->
            { state
                | processes = Set.remove pt state.processes
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

        AddedGroupType gt ->
            { state
                | groupTypes = Set.insert gt state.groupTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        RemovedGroupType gt ->
            { state
                | groupTypes = Set.remove gt state.groupTypes
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
                | grouped = Set.insert (GroupLink.Link e g) state.grouped
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Set.insert b.uuid state.uuids
            }

        Ungrouped e g ->
            { state
                | grouped = Set.remove (GroupLink.Link e g) state.grouped
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


allTyped : State -> TType.Type -> DictSet String (Typed OnlyItem)
allTyped s t =
    case t of
        TType.Resource ->
            s.resources |> Set.map Item.compare (\x -> { what = x.what, uuid = x.uuid, type_ = x.type_ })

        TType.Event ->
            s.events |> Set.map Item.compare (\x -> { what = x.what, uuid = x.uuid, type_ = x.type_ })

        TType.Agent ->
            s.agents |> Set.map Item.compare (\x -> { what = x.what, uuid = x.uuid, type_ = x.type_ })

        TType.Commitment ->
            s.commitments |> Set.map Item.compare (\x -> { what = x.what, uuid = x.uuid, type_ = x.type_ })

        TType.Contract ->
            s.contracts |> Set.map Item.compare (\x -> { what = x.what, uuid = x.uuid, type_ = x.type_ })

        TType.Process ->
            s.processes |> Set.map Item.compare (\x -> { what = x.what, uuid = x.uuid, type_ = x.type_ })

        TType.Group ->
            s.groups |> Set.map Item.compare (\x -> { what = x.what, uuid = x.uuid, type_ = x.type_ })


allHierarchic : State -> HType.Type -> DictSet String (Hierarchic OnlyItem)
allHierarchic s t =
    case t of
        HType.ResourceType ->
            s.resourceTypes |> Set.map Item.compare (\x -> { what = x.what, uuid = x.uuid, parent = x.parent })

        HType.EventType ->
            s.eventTypes |> Set.map Item.compare (\x -> { what = x.what, uuid = x.uuid, parent = x.parent })

        HType.AgentType ->
            s.agentTypes |> Set.map Item.compare (\x -> { what = x.what, uuid = x.uuid, parent = x.parent })

        HType.CommitmentType ->
            s.commitmentTypes |> Set.map Item.compare (\x -> { what = x.what, uuid = x.uuid, parent = x.parent })

        HType.ContractType ->
            s.contractTypes |> Set.map Item.compare (\x -> { what = x.what, uuid = x.uuid, parent = x.parent })

        HType.ProcessType ->
            s.processTypes |> Set.map Item.compare (\x -> { what = x.what, uuid = x.uuid, parent = x.parent })

        HType.GroupType ->
            s.groupTypes |> Set.map Item.compare (\x -> { what = x.what, uuid = x.uuid, parent = Nothing })
