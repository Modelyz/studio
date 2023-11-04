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
import Message exposing (Message(..), messageId)
import MessageFlow exposing (MessageFlow(..))
import MessageId exposing (MessageId)
import Payload exposing (Payload(..))
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Process exposing (Process)
import Process.Reconcile as Reconcile exposing (Reconciliation)
import ProcessType.ProcessType exposing (ProcessType)
import Relation.ProcessCommitments exposing (ProcessCommitments)
import Relation.ProcessEvents exposing (ProcessEvents)
import Resource.Resource as Resource exposing (Resource)
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
    { pendingMessages : Dict String Message -- pending Messages are in Requested mode. TODO: try to reconstruct the pendingMessages from idb when needed, and just maintain a nb of pending messages
    , syncing : Bool

    {- probably unusedful: -}
    , lastMessageTime : Time.Posix -- time of the last message
    , uuids : Dict String MessageId -- the uuids of all messages

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
    , syncing = True
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
aggregate ((Message m p) as msg) state =
    let
        mid =
            messageId msg

        commonState =
            { state
                | lastMessageTime = m.when
                , pendingMessages = updatePending msg state.pendingMessages
                , uuids = Dict.insert (MessageId.compare mid) mid state.uuids
            }
    in
    case p of
        Null ->
            state

        InitiatedConnection _ ->
            case m.flow of
                Processed ->
                    { commonState | syncing = False }

                _ ->
                    commonState

        AddedIdentifierType it ->
            { commonState
                | identifierTypes = Dict.insert (IdentifierType.compare it) it state.identifierTypes
            }

        ChangedIdentifierType c ->
            { commonState
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
            { commonState
                | identifierTypes = Dict.remove (IdentifierType.compare it) state.identifierTypes
                , identifiers =
                    -- keep the identifiers whose name are different from the one removed,
                    -- or if this is the same name, whose item is not in the scope of the identifier type
                    state.identifiers
                        |> Dict.filter
                            (\_ i -> i.name /= it.name || not (containsScope state.types (IsItem i.what i.for) it.scope))
            }

        AddedValueType it ->
            { commonState
                | valueTypes = Dict.insert (ValueType.compare it) it state.valueTypes
            }

        ChangedValueType c ->
            { commonState
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
            { commonState
                | valueTypes = Dict.remove (ValueType.compare vt) state.valueTypes
                , values =
                    -- keep the values whose name are different from the one removed,
                    -- or if this is the same name, whose item is not in the scope of the value type
                    state.values
                        |> Dict.filter
                            (\_ v -> (v.name /= vt.name) || not (containsScope state.types (IsItem v.what v.for) vt.scope))
            }

        AddedValue v ->
            { commonState
                | values = Dict.insert (Value.compare v) v state.values
            }

        AddedResourceType rt ->
            { commonState
                | resourceTypes = Dict.insert (Uuid.toString rt.uuid) rt state.resourceTypes
                , types = Dict.insert (Uuid.toString rt.uuid) ( rt.uuid, Type.HType HType.ResourceType, rt.parent ) state.types
            }

        RemovedResourceType uuid ->
            { commonState
                | resourceTypes = Dict.remove (Uuid.toString uuid) state.resourceTypes
                , types = Dict.remove (Uuid.toString uuid) state.types
            }

        AddedEventType et ->
            { commonState
                | eventTypes = Dict.insert (Uuid.toString et.uuid) et state.eventTypes
                , types = Dict.insert (Uuid.toString et.uuid) ( et.uuid, Type.HType HType.EventType, et.parent ) state.types
            }

        RemovedEventType uuid ->
            { commonState
                | eventTypes = Dict.remove (Uuid.toString uuid) state.eventTypes
                , types = Dict.remove (Uuid.toString uuid) state.types
            }

        AddedAgentType at ->
            { commonState
                | agentTypes = Dict.insert (Uuid.toString at.uuid) at state.agentTypes
                , types = Dict.insert (Uuid.toString at.uuid) ( at.uuid, Type.HType HType.AgentType, at.parent ) state.types
            }

        RemovedAgentType uuid ->
            { commonState
                | agentTypes = Dict.remove (Uuid.toString uuid) state.agentTypes
                , types = Dict.remove (Uuid.toString uuid) state.types
            }

        AddedCommitmentType cmt ->
            { commonState
                | commitmentTypes = Dict.insert (Uuid.toString cmt.uuid) cmt state.commitmentTypes
                , types = Dict.insert (Uuid.toString cmt.uuid) ( cmt.uuid, Type.HType HType.CommitmentType, cmt.parent ) state.types
            }

        RemovedCommitmentType uuid ->
            { commonState
                | commitmentTypes = Dict.remove (Uuid.toString uuid) state.commitmentTypes
                , types = Dict.remove (Uuid.toString uuid) state.types
            }

        AddedContractType cnt ->
            { commonState
                | contractTypes = Dict.insert (Uuid.toString cnt.uuid) cnt state.contractTypes
                , types = Dict.insert (Uuid.toString cnt.uuid) ( cnt.uuid, Type.HType HType.ContractType, cnt.parent ) state.types
            }

        RemovedContractType uuid ->
            { commonState
                | contractTypes = Dict.remove (Uuid.toString uuid) state.contractTypes
                , types = Dict.remove (Uuid.toString uuid) state.types
            }

        AddedProcessType pt ->
            { commonState
                | processTypes = Dict.insert (Uuid.toString pt.uuid) pt state.processTypes
                , types = Dict.insert (Uuid.toString pt.uuid) ( pt.uuid, Type.HType HType.ProcessType, pt.parent ) state.types
            }

        RemovedProcessType uuid ->
            { commonState
                | processTypes = Dict.remove (Uuid.toString uuid) state.processTypes
                , types = Dict.remove (Uuid.toString uuid) state.types
            }

        AddedResource r ->
            { commonState
                | resources = Dict.insert (Resource.compare r) r state.resources
                , types = Dict.insert (Uuid.toString r.uuid) ( r.uuid, Type.TType TType.Resource, Just r.type_ ) state.types
            }

        RemovedResource uuid ->
            { commonState
                | resources = Dict.remove (Uuid.toString uuid) state.resources
                , types = Dict.remove (Uuid.toString uuid) state.types
            }

        AddedEvent e ->
            { commonState
                | events = Dict.insert (Uuid.toString e.uuid) e state.events
                , types = Dict.insert (Uuid.toString e.uuid) ( e.uuid, Type.TType TType.Event, Just e.type_ ) state.types
            }

        RemovedEvent uuid ->
            { commonState
                | events = Dict.remove (Uuid.toString uuid) state.events
                , types = Dict.remove (Uuid.toString uuid) state.types
            }

        AddedAgent a ->
            { commonState
                | agents = Dict.insert (Uuid.toString a.uuid) a state.agents
                , types = Dict.insert (Uuid.toString a.uuid) ( a.uuid, Type.TType TType.Agent, Just a.type_ ) state.types
            }

        RemovedAgent uuid ->
            { commonState
                | agents = Dict.remove (Uuid.toString uuid) state.agents
                , types = Dict.remove (Uuid.toString uuid) state.types
            }

        AddedCommitment cm ->
            { commonState
                | commitments = Dict.insert (Uuid.toString cm.uuid) cm state.commitments
                , types = Dict.insert (Uuid.toString cm.uuid) ( cm.uuid, Type.TType TType.Commitment, Just cm.type_ ) state.types
            }

        RemovedCommitment uuid ->
            { commonState
                | commitments = Dict.remove (Uuid.toString uuid) state.commitments
                , types = Dict.remove (Uuid.toString uuid) state.types
            }

        AddedContract cn ->
            { commonState
                | contracts = Dict.insert (Uuid.toString cn.uuid) cn state.contracts
                , types = Dict.insert (Uuid.toString cn.uuid) ( cn.uuid, Type.TType TType.Contract, Just cn.type_ ) state.types
            }

        RemovedContract uuid ->
            { commonState
                | contracts = Dict.remove (Uuid.toString uuid) state.contracts
                , types = Dict.remove (Uuid.toString uuid) state.types
            }

        AddedProcess pr ->
            { commonState
                | processes = Dict.insert (Uuid.toString pr.uuid) pr state.processes
                , types = Dict.insert (Uuid.toString pr.uuid) ( pr.uuid, Type.TType TType.Process, Just pr.type_ ) state.types
            }

        RemovedProcess uuid ->
            { commonState
                | processes = Dict.remove (Uuid.toString uuid) state.processes
                , reconciliations = Dict.filter (\_ r -> r.process /= uuid) state.reconciliations
                , types = Dict.remove (Uuid.toString uuid) state.types
            }

        AddedIdentifier ei ->
            { commonState
                | identifiers = Dict.insert (Identifier.compare ei) ei state.identifiers
            }

        Configured conf ->
            { commonState
                | configs = Dict.insert (Configuration.compare conf) conf state.configs
            }

        Unconfigured conf ->
            { commonState
                | configs = Dict.remove (Configuration.compare conf) state.configs
            }

        AddedGroupType gt ->
            { commonState
                | groupTypes = Dict.insert (Uuid.toString gt.uuid) gt state.groupTypes
                , types = Dict.insert (Uuid.toString gt.uuid) ( gt.uuid, Type.HType HType.GroupType, gt.parent ) state.types
            }

        RemovedGroupType uuid ->
            { commonState
                | groupTypes = Dict.remove (Uuid.toString uuid) state.groupTypes
                , types = Dict.remove (Uuid.toString uuid) state.types
            }

        DefinedGroup g ->
            { commonState
                | groups = Dict.insert (Uuid.toString g.uuid) g state.groups
                , types = Dict.insert (Uuid.toString g.uuid) ( g.uuid, Type.TType TType.Group, Just g.type_ ) state.types
            }

        RemovedGroup uuid ->
            { commonState
                | groups = Dict.remove (Uuid.toString uuid) state.groups
                , types = Dict.remove (Uuid.toString uuid) state.types
            }

        Grouped link ->
            { commonState
                | grouped = Dict.insert (GroupLink.compare link) link state.grouped
            }

        Ungrouped link ->
            { commonState
                | grouped = Dict.remove (GroupLink.compare link) state.grouped
            }

        Reconciled reconciliation ->
            { commonState
                | reconciliations = Dict.insert (Reconcile.compare reconciliation) reconciliation state.reconciliations
            }

        Unreconciled reconciliation ->
            { commonState
                | reconciliations = Dict.remove (Reconcile.compare reconciliation) state.reconciliations
            }


updatePending : Message -> Dict String Message -> Dict String Message
updatePending (Message m p) ms =
    case m.flow of
        Requested ->
            Dict.insert (Message.compare (Message m p)) (Message m p) ms

        Processed ->
            Dict.remove (Message.compare (Message m p)) ms

        Error ->
            Dict.insert (Message.compare (Message m p)) (Message m p) ms
