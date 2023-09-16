module State exposing (State, aggregate, empty)

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
import Hierarchy.Type as HType
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType as IdentifierType exposing (IdentifierType)
import Message exposing (Message(..))
import MessageFlow exposing (MessageFlow(..))
import Metadata exposing (Metadata)
import Payload exposing (Payload(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Process exposing (Process)
import Process.Reconcile as Reconcile exposing (Reconciliation)
import ProcessType.ProcessType exposing (ProcessType)
import Relation.ProcessCommitments exposing (ProcessCommitments)
import Relation.ProcessEvents exposing (ProcessEvents)
import Resource.Resource exposing (Resource)
import ResourceType.ResourceType exposing (ResourceType)
import Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import Time exposing (millisToPosix)
import Type exposing (Type)
import Typed.Type as TType
import Value.Value as Value exposing (Value)
import Value.ValueType as ValueType exposing (ValueType)


type alias State =
    -- Reminder: Dicts in the State are actually meant to bu used as Sets
    -- using a relevant comparable so that each entity can appear only once
    { pendingMessages : Dict Int Message -- pending Messages are in Requested mode. TODO: try to reconstruct the pendingMessages from idb when needed, and just maintain a nb of pending messages

    {- probably unusedful: -}
    , lastMessageTime : Time.Posix -- time of the last message
    , uuids : Dict String Metadata -- the uuids of all messages

    -- entities
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

    -- Hierarchy mapping from an entity uuid to its own type and parent type uuid
    -- TODO turn to a Dict String Type or Something
    , types : Dict String ( Uuid, Type, Maybe Uuid )

    -- links
    , process_commitments : Dict String ProcessCommitments
    , process_events : Dict String ProcessEvents
    , grouped : Dict String GroupLink.Link
    , reconciliations : Dict String Reconciliation

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
    , types = Dict.empty
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
    , reconciliations = Dict.empty

    -- behaviours
    , identifierTypes = Dict.empty
    , identifiers = Dict.empty
    , valueTypes = Dict.empty
    , values = Dict.empty

    -- config
    , configs = Dict.empty
    }


aggregate : Message -> State -> State
aggregate (Message m p) state =
    case p of
        Null ->
            state

        InitiatedConnection _ ->
            { state
                | lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        AddedIdentifierType it ->
            { state
                | identifierTypes = Dict.insert (IdentifierType.compare it) it state.identifierTypes
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        ChangedIdentifierType c ->
            { state
                | identifierTypes =
                    Dict.insert (IdentifierType.compare c.new) c.new <|
                        Dict.remove (IdentifierType.compare c.old) state.identifierTypes
                , identifiers =
                    -- change the name of all the identifiers whose name equals the new name of the identifier type
                    -- and that are in the scope of the changed identifier type
                    state.identifiers
                        |> Dict.values
                        |> List.map
                            -- rebuild the identifier compare keys
                            (\i ->
                                if i.name == c.old.name && containsScope state.types (IsItem i.what i.for) c.new.scope then
                                    let
                                        newi =
                                            { i | name = c.new.name }
                                    in
                                    ( Identifier.compare newi, newi )

                                else
                                    ( Identifier.compare i, i )
                            )
                        |> Dict.fromList
            }

        RemovedIdentifierType it ->
            { state
                | identifierTypes = Dict.remove (IdentifierType.compare it) state.identifierTypes
                , identifiers =
                    -- keep the identifiers whose name are different from the one removed,
                    -- or if this is the same name, whose item is not in the scope of the identifier type
                    state.identifiers
                        |> Dict.filter
                            (\_ i -> i.name /= it.name || not (containsScope state.types (IsItem i.what i.for) it.scope))
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        AddedValueType it ->
            { state
                | valueTypes = Dict.insert (ValueType.compare it) it state.valueTypes
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        ChangedValueType c ->
            { state
                | valueTypes =
                    Dict.insert (ValueType.compare c.new) c.new <|
                        Dict.remove (ValueType.compare c.old) state.valueTypes
                , values =
                    -- change the name of all the values whose name equals the new name of the identifier type
                    state.values
                        |> Dict.values
                        |> List.map
                            -- rebuild the value compare keys
                            (\v ->
                                if v.name == c.old.name && containsScope state.types (IsItem v.what v.for) c.new.scope then
                                    let
                                        newi =
                                            { v | name = c.new.name }
                                    in
                                    ( Value.compare newi, newi )

                                else
                                    ( Value.compare v, v )
                            )
                        |> Dict.fromList
            }

        RemovedValueType vt ->
            { state
                | valueTypes = Dict.remove (ValueType.compare vt) state.valueTypes
                , values =
                    -- keep the values whose name are different from the one removed,
                    -- or if this is the same name, whose item is not in the scope of the value type
                    state.values
                        |> Dict.filter
                            (\_ v -> (v.name /= vt.name) || not (containsScope state.types (IsItem v.what v.for) vt.scope))
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        AddedValue v ->
            { state
                | values = Dict.insert (Value.compare v) v state.values
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        AddedResourceType rt ->
            { state
                | resourceTypes = Dict.insert (Uuid.toString rt.uuid) rt state.resourceTypes
                , types = Dict.insert (Uuid.toString rt.uuid) ( rt.uuid, Type.HType HType.ResourceType, rt.parent ) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        RemovedResourceType uuid ->
            { state
                | resourceTypes = Dict.remove (Uuid.toString uuid) state.resourceTypes
                , types = Dict.remove (Uuid.toString uuid) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        AddedEventType et ->
            { state
                | eventTypes = Dict.insert (Uuid.toString et.uuid) et state.eventTypes
                , types = Dict.insert (Uuid.toString et.uuid) ( et.uuid, Type.HType HType.EventType, et.parent ) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        RemovedEventType uuid ->
            { state
                | eventTypes = Dict.remove (Uuid.toString uuid) state.eventTypes
                , types = Dict.remove (Uuid.toString uuid) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        AddedAgentType at ->
            { state
                | agentTypes = Dict.insert (Uuid.toString at.uuid) at state.agentTypes
                , types = Dict.insert (Uuid.toString at.uuid) ( at.uuid, Type.HType HType.AgentType, at.parent ) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        RemovedAgentType uuid ->
            { state
                | agentTypes = Dict.remove (Uuid.toString uuid) state.agentTypes
                , types = Dict.remove (Uuid.toString uuid) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        AddedCommitmentType cmt ->
            { state
                | commitmentTypes = Dict.insert (Uuid.toString cmt.uuid) cmt state.commitmentTypes
                , types = Dict.insert (Uuid.toString cmt.uuid) ( cmt.uuid, Type.HType HType.CommitmentType, cmt.parent ) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        RemovedCommitmentType uuid ->
            { state
                | commitmentTypes = Dict.remove (Uuid.toString uuid) state.commitmentTypes
                , types = Dict.remove (Uuid.toString uuid) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        AddedContractType cnt ->
            { state
                | contractTypes = Dict.insert (Uuid.toString cnt.uuid) cnt state.contractTypes
                , types = Dict.insert (Uuid.toString cnt.uuid) ( cnt.uuid, Type.HType HType.ContractType, cnt.parent ) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        RemovedContractType uuid ->
            { state
                | contractTypes = Dict.remove (Uuid.toString uuid) state.contractTypes
                , types = Dict.remove (Uuid.toString uuid) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        AddedProcessType pt ->
            { state
                | processTypes = Dict.insert (Uuid.toString pt.uuid) pt state.processTypes
                , types = Dict.insert (Uuid.toString pt.uuid) ( pt.uuid, Type.HType HType.ProcessType, pt.parent ) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        RemovedProcessType uuid ->
            { state
                | processTypes = Dict.remove (Uuid.toString uuid) state.processTypes
                , types = Dict.remove (Uuid.toString uuid) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        AddedResource r ->
            { state
                | resources = Dict.insert (Uuid.toString r.uuid) r state.resources
                , types = Dict.insert (Uuid.toString r.uuid) ( r.uuid, Type.TType TType.Resource, Just r.type_ ) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        RemovedResource uuid ->
            { state
                | resources = Dict.remove (Uuid.toString uuid) state.resources
                , types = Dict.remove (Uuid.toString uuid) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        AddedEvent e ->
            { state
                | events = Dict.insert (Uuid.toString e.uuid) e state.events
                , types = Dict.insert (Uuid.toString e.uuid) ( e.uuid, Type.TType TType.Event, Just e.type_ ) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        RemovedEvent uuid ->
            { state
                | events = Dict.remove (Uuid.toString uuid) state.events
                , types = Dict.remove (Uuid.toString uuid) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        AddedAgent a ->
            { state
                | agents = Dict.insert (Uuid.toString a.uuid) a state.agents
                , types = Dict.insert (Uuid.toString a.uuid) ( a.uuid, Type.TType TType.Agent, Just a.type_ ) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        RemovedAgent uuid ->
            { state
                | agents = Dict.remove (Uuid.toString uuid) state.agents
                , types = Dict.remove (Uuid.toString uuid) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        AddedCommitment cm ->
            { state
                | commitments = Dict.insert (Uuid.toString cm.uuid) cm state.commitments
                , types = Dict.insert (Uuid.toString cm.uuid) ( cm.uuid, Type.TType TType.Commitment, Just cm.type_ ) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        RemovedCommitment uuid ->
            { state
                | commitments = Dict.remove (Uuid.toString uuid) state.commitments
                , types = Dict.remove (Uuid.toString uuid) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        AddedContract cn ->
            { state
                | contracts = Dict.insert (Uuid.toString cn.uuid) cn state.contracts
                , types = Dict.insert (Uuid.toString cn.uuid) ( cn.uuid, Type.TType TType.Contract, Just cn.type_ ) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        RemovedContract uuid ->
            { state
                | contracts = Dict.remove (Uuid.toString uuid) state.contracts
                , types = Dict.remove (Uuid.toString uuid) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        AddedProcess pr ->
            { state
                | processes = Dict.insert (Uuid.toString pr.uuid) pr state.processes
                , types = Dict.insert (Uuid.toString pr.uuid) ( pr.uuid, Type.TType TType.Process, Just pr.type_ ) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        RemovedProcess uuid ->
            { state
                | processes = Dict.remove (Uuid.toString uuid) state.processes
                , reconciliations = Dict.filter (\_ r -> r.process /= uuid) state.reconciliations
                , types = Dict.remove (Uuid.toString uuid) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        AddedIdentifier ei ->
            { state
                | identifiers = Dict.insert (Identifier.compare ei) ei state.identifiers
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        Configured conf ->
            { state
                | configs = Dict.insert (Configuration.compare conf) conf state.configs
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        Unconfigured conf ->
            { state
                | configs = Dict.remove (Configuration.compare conf) state.configs
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        AddedGroupType gt ->
            { state
                | groupTypes = Dict.insert (Uuid.toString gt.uuid) gt state.groupTypes
                , types = Dict.insert (Uuid.toString gt.uuid) ( gt.uuid, Type.HType HType.GroupType, gt.parent ) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        RemovedGroupType uuid ->
            { state
                | groupTypes = Dict.remove (Uuid.toString uuid) state.groupTypes
                , types = Dict.remove (Uuid.toString uuid) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        DefinedGroup g ->
            { state
                | groups = Dict.insert (Uuid.toString g.uuid) g state.groups
                , types = Dict.insert (Uuid.toString g.uuid) ( g.uuid, Type.TType TType.Group, Just g.type_ ) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        RemovedGroup uuid ->
            { state
                | groups = Dict.remove (Uuid.toString uuid) state.groups
                , types = Dict.remove (Uuid.toString uuid) state.types
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        Grouped link ->
            { state
                | grouped = Dict.insert (GroupLink.compare link) link state.grouped
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        Ungrouped link ->
            { state
                | grouped = Dict.remove (GroupLink.compare link) state.grouped
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        Reconciled reconciliation ->
            { state
                | reconciliations = Dict.insert (Reconcile.compare reconciliation) reconciliation state.reconciliations
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }

        Unreconciled reconciliation ->
            { state
                | reconciliations = Dict.remove (Reconcile.compare reconciliation) state.reconciliations
                , lastMessageTime = m.when
                , pendingMessages = updatePending (Message m p) state.pendingMessages
                , uuids = Dict.insert (Metadata.compare m) m state.uuids
            }


updatePending : Message -> Dict Int Message -> Dict Int Message
updatePending (Message m p) ms =
    case m.flow of
        Requested ->
            Dict.insert (Message.compare (Message m p)) (Message m p) ms

        Processed ->
            Dict.remove (Message.compare (Message m p)) ms

        Error _ ->
            Dict.insert (Message.compare (Message m p)) (Message m p) ms
