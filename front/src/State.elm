module State exposing (State, aggregate, allHierarchic, allTyped, empty)

import Agent.Agent exposing (Agent)
import AgentType.AgentType exposing (AgentType)
import Commitment.Commitment exposing (Commitment)
import CommitmentType.CommitmentType exposing (CommitmentType)
import Configuration exposing (Configuration)
import Contract.Contract exposing (Contract)
import ContractType.ContractType exposing (ContractType)
import Dict exposing (Dict)
import Event.Event exposing (Event)
import EventType.EventType exposing (EventType)
import Group.Group exposing (Group)
import Group.Link as GroupLink
import GroupType.GroupType exposing (GroupType)
import Hierarchy.Hierarchic as H
import Hierarchy.Type as HType
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType as IdentifierType exposing (IdentifierType)
import Item.Item exposing (Item)
import Message exposing (Message(..), Payload(..), base)
import MessageFlow exposing (MessageFlow(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Process exposing (Process)
import ProcessType.ProcessType exposing (ProcessType)
import Relation.ProcessCommitments exposing (ProcessCommitments)
import Relation.ProcessEvents exposing (ProcessEvents)
import Resource.Resource exposing (Resource)
import ResourceType.ResourceType exposing (ResourceType)
import Scope.Scope exposing (containsItem)
import Time exposing (millisToPosix)
import Type
import Typed.Type as TType
import Typed.Typed as T exposing (OnlyTyped)
import Value.Value as Value exposing (Value)
import Value.ValueType as ValueType exposing (ValueType)


type alias State =
    { pendingMessages : Dict Int Message
    , lastMessageTime : Time.Posix
    , uuids : Dict String Uuid

    -- entities
    --, entities : Dict String Entity
    , resources : Dict String Resource
    , events : Dict String Event
    , agents : Dict String Agent
    , commitments : Dict String Commitment
    , resourceTypes : Dict String ResourceType
    , eventTypes : Dict String EventType
    , agentTypes : Dict String AgentType
    , commitmentTypes : Dict String CommitmentType
    , contracts : Dict String Contract
    , contractTypes : Dict String ContractType
    , processes : Dict String Process
    , processTypes : Dict String ProcessType
    , groups : Dict String Group
    , groupTypes : Dict String GroupType

    -- links
    , process_commitments : Dict String ProcessCommitments
    , process_events : Dict String ProcessEvents
    , grouped : Dict String GroupLink.Link

    -- ident
    , identifierTypes : Dict String IdentifierType
    , identifiers : Dict String Identifier

    -- value
    , valueTypes : Dict String ValueType
    , values : Dict String Value

    -- config
    , configs : Dict String Configuration
    }


empty : State
empty =
    { pendingMessages = Dict.empty
    , lastMessageTime = millisToPosix 0
    , uuids = Dict.empty

    -- entities
    --, entities = Dict.empty
    , resources = Dict.empty
    , events = Dict.empty
    , agents = Dict.empty
    , commitments = Dict.empty
    , resourceTypes = Dict.empty
    , eventTypes = Dict.empty
    , agentTypes = Dict.empty
    , commitmentTypes = Dict.empty
    , contracts = Dict.empty
    , contractTypes = Dict.empty
    , processes = Dict.empty
    , processTypes = Dict.empty
    , groups = Dict.empty
    , groupTypes = Dict.empty

    -- links
    , process_events = Dict.empty
    , process_commitments = Dict.empty
    , grouped = Dict.empty

    -- behaviours
    , identifierTypes = Dict.empty
    , identifiers = Dict.empty
    , valueTypes = Dict.empty
    , values = Dict.empty

    -- config
    , configs = Dict.empty
    }


insertItem : Item a -> Dict String (Item a) -> Dict String (Item a)
insertItem i d =
    Dict.insert (Uuid.toString i.uuid) i d


removeUuid : Uuid -> Dict String (Item a) -> Dict String (Item a)
removeUuid uuid d =
    Dict.remove (Uuid.toString uuid) d


insertUuid : Uuid -> Dict String Uuid -> Dict String Uuid
insertUuid uuid =
    Dict.insert (Uuid.toString uuid) uuid


aggregate : Message -> State -> State
aggregate (Message b p) state =
    case p of
        -- TODO review the names (some are StuffAdded, some AddedStuff)
        ConnectionInitiated _ ->
            { state
                | lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Dict.insert (Uuid.toString b.uuid) b.uuid state.uuids
            }

        IdentifierTypeAdded it ->
            { state
                | identifierTypes = Dict.insert (IdentifierType.compare it) it state.identifierTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Dict.insert (Uuid.toString b.uuid) b.uuid state.uuids
            }

        IdentifierTypeRemoved it ->
            { state
                | identifierTypes = Dict.remove (IdentifierType.compare it) state.identifierTypes
                , identifiers =
                    -- keep the identifiers whose name are different from the one removed,
                    -- or if this is the same name, whose item is not in the scope of the identifier type
                    state.identifiers
                        |> Dict.filter
                            (\_ i ->
                                i.name
                                    /= it.name
                                    || not
                                        ((case i.what of
                                            Type.TType tt ->
                                                T.find (allTyped state tt) i.identifiable
                                                    |> Maybe.map (containsItem it.applyTo)

                                            Type.HType ht ->
                                                H.find (allHierarchic state ht) i.identifiable
                                                    |> Maybe.map (containsItem it.applyTo)
                                         )
                                            |> Maybe.withDefault False
                                        )
                            )
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Dict.insert (Uuid.toString b.uuid) b.uuid state.uuids
            }

        ValueTypeAdded it ->
            { state
                | valueTypes = Dict.insert (ValueType.compare it) it state.valueTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Dict.insert (Uuid.toString b.uuid) b.uuid state.uuids
            }

        ValueTypeRemoved vt ->
            { state
                | valueTypes = Dict.remove (ValueType.compare vt) state.valueTypes
                , values =
                    -- keep the values whose name are different from the one removed,
                    -- or if this is the same name, whose item is not in the scope of the value type
                    state.values
                        |> Dict.filter
                            (\_ v ->
                                v.name
                                    /= vt.name
                                    || not
                                        ((case v.what of
                                            Type.TType tt ->
                                                T.find (allTyped state tt) v.for
                                                    |> Maybe.map (containsItem vt.scope)

                                            Type.HType ht ->
                                                H.find (allHierarchic state ht) v.for
                                                    |> Maybe.map (containsItem vt.scope)
                                         )
                                            |> Maybe.withDefault False
                                        )
                            )
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Dict.insert (Uuid.toString b.uuid) b.uuid state.uuids
            }

        ValueAdded v ->
            { state
                | values = Dict.insert (Value.compare v) v state.values
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedResourceType rt ->
            { state
                | resourceTypes = insertItem rt state.resourceTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedResourceType uuid ->
            { state
                | resourceTypes = removeUuid uuid state.resourceTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedEventType et ->
            { state
                | eventTypes = insertItem et state.eventTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedEventType uuid ->
            { state
                | eventTypes = removeUuid uuid state.eventTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedAgentType at ->
            { state
                | agentTypes = insertItem at state.agentTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedAgentType uuid ->
            { state
                | agentTypes = removeUuid uuid state.agentTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedCommitmentType cmt ->
            { state
                | commitmentTypes = insertItem cmt state.commitmentTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedCommitmentType uuid ->
            { state
                | commitmentTypes = removeUuid uuid state.commitmentTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedContractType cnt ->
            { state
                | contractTypes = insertItem cnt state.contractTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedContractType uuid ->
            { state
                | contractTypes = removeUuid uuid state.contractTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedProcessType pt ->
            { state
                | processTypes = insertItem pt state.processTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedProcessType uuid ->
            { state
                | processTypes = removeUuid uuid state.processTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedResource rt ->
            { state
                | resources = insertItem rt state.resources
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedResource uuid ->
            { state
                | resources = removeUuid uuid state.resources
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedEvent et ->
            { state
                | events = insertItem et state.events
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedEvent uuid ->
            { state
                | events = removeUuid uuid state.events
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedAgent at ->
            { state
                | agents = insertItem at state.agents
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedAgent uuid ->
            { state
                | agents = removeUuid uuid state.agents
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedCommitment cmt ->
            { state
                | commitments = insertItem cmt state.commitments
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedCommitment uuid ->
            { state
                | commitments = removeUuid uuid state.commitments
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedContract cnt ->
            { state
                | contracts = insertItem cnt state.contracts
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedContract uuid ->
            { state
                | contracts = removeUuid uuid state.contracts
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedProcess pt ->
            { state
                | processes = insertItem pt state.processes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedProcess uuid ->
            { state
                | processes = removeUuid uuid state.processes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        IdentifierAdded ei ->
            { state
                | identifiers = Dict.insert (Identifier.compare ei) ei state.identifiers
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        Configured conf ->
            { state
                | configs = Dict.insert (Configuration.compare conf) conf state.configs
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        Unconfigured conf ->
            { state
                | configs = Dict.remove (Configuration.compare conf) state.configs
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedGroupType gt ->
            { state
                | groupTypes = insertItem gt state.groupTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedGroupType uuid ->
            { state
                | groupTypes = removeUuid uuid state.groupTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        DefinedGroup group ->
            { state
                | groups = insertItem group state.groups
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedGroup uuid ->
            { state
                | groups = removeUuid uuid state.groups
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        Grouped e g ->
            let
                grouplink =
                    GroupLink.Link e g
            in
            { state
                | grouped = Dict.insert (GroupLink.compare grouplink) grouplink state.grouped
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        Ungrouped e g ->
            let
                grouplink =
                    GroupLink.Link e g
            in
            { state
                | grouped = Dict.remove (GroupLink.compare grouplink) state.grouped
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }


updatePending : Message -> Dict Int Message -> Dict Int Message
updatePending e es =
    case .flow <| base <| e of
        Requested ->
            Dict.insert (Message.compare e) e es

        Processed ->
            Dict.remove (Message.compare e) es


allTyped : State -> TType.Type -> Dict String OnlyTyped
allTyped s t =
    case t of
        TType.Resource ->
            s.resources |> Dict.map (\_ x -> { what = x.what, uuid = x.uuid, type_ = x.type_, identifiers = Dict.empty, values = Dict.empty, display = Dict.empty })

        TType.Event ->
            s.events |> Dict.map (\_ x -> { what = x.what, uuid = x.uuid, type_ = x.type_, identifiers = Dict.empty, values = Dict.empty, display = Dict.empty })

        TType.Agent ->
            s.agents |> Dict.map (\_ x -> { what = x.what, uuid = x.uuid, type_ = x.type_, identifiers = Dict.empty, values = Dict.empty, display = Dict.empty })

        TType.Commitment ->
            s.commitments |> Dict.map (\_ x -> { what = x.what, uuid = x.uuid, type_ = x.type_, identifiers = Dict.empty, values = Dict.empty, display = Dict.empty })

        TType.Contract ->
            s.contracts |> Dict.map (\_ x -> { what = x.what, uuid = x.uuid, type_ = x.type_, identifiers = Dict.empty, values = Dict.empty, display = Dict.empty })

        TType.Process ->
            s.processes |> Dict.map (\_ x -> { what = x.what, uuid = x.uuid, type_ = x.type_, identifiers = Dict.empty, values = Dict.empty, display = Dict.empty })

        TType.Group ->
            s.groups |> Dict.map (\_ x -> { what = x.what, uuid = x.uuid, type_ = x.type_, identifiers = Dict.empty, values = Dict.empty, display = Dict.empty })



--allHierarchic : State -> HType.Type -> Dict String ((Hierarchic a))
-- TODO annotation?
-- TODO this function fails to compile if all the entity types are not homomorphic. If one field is added in one, it must be added in all


allHierarchic s t =
    case t of
        HType.ResourceType ->
            s.resourceTypes

        HType.EventType ->
            s.eventTypes

        HType.AgentType ->
            s.agentTypes

        HType.CommitmentType ->
            s.commitmentTypes

        HType.ContractType ->
            s.contractTypes

        HType.ProcessType ->
            s.processTypes

        HType.GroupType ->
            s.groupTypes
