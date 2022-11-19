module State exposing (State, aggregate, allHfromScope, allHierarchic, allTfromScope, allTyped, empty)

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
import Group.WithGroups exposing (WithGroups)
import GroupType.GroupType exposing (GroupType)
import Hierarchy.Hierarchic as H exposing (Hierarchic)
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
import Scope.Scope exposing (Scope(..))
import Scope.State exposing (containsScope)
import Time exposing (millisToPosix)
import Type
import Typed.Type as TType
import Typed.Typed as T exposing (OnlyTyped, Typed)
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


insertT : Typed a -> Dict String (Typed a) -> Dict String (Typed a)
insertT i d =
    Dict.insert (Uuid.toString i.uuid) i d


insertH : Hierarchic a -> Dict String (Hierarchic a) -> Dict String (Hierarchic a)
insertH i d =
    Dict.insert (Uuid.toString i.uuid) i d


removeHUuid : Uuid -> Dict String (Hierarchic a) -> Dict String (Hierarchic a)
removeHUuid uuid d =
    Dict.remove (Uuid.toString uuid) d


removeTUuid : Uuid -> Dict String (Typed a) -> Dict String (Typed a)
removeTUuid uuid d =
    Dict.remove (Uuid.toString uuid) d


insertUuid : Uuid -> Dict String Uuid -> Dict String Uuid
insertUuid uuid =
    Dict.insert (Uuid.toString uuid) uuid


aggregate : Message -> State -> State
aggregate (Message b p) state =
    case p of
        InitiatedConnection _ ->
            { state
                | lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Dict.insert (Uuid.toString b.uuid) b.uuid state.uuids
            }

        AddedIdentifierType it ->
            { state
                | identifierTypes = Dict.insert (IdentifierType.compare it) it state.identifierTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Dict.insert (Uuid.toString b.uuid) b.uuid state.uuids
            }

        ChangedIdentifierType new old ->
            { state
                | identifierTypes =
                    Dict.insert (IdentifierType.compare new) new <|
                        Dict.remove (IdentifierType.compare old) state.identifierTypes
                , identifiers =
                    -- change the name of all the identifiers whose name equals the new name of the identifier type
                    -- and that are in the scope of the changed identifier type
                    state.identifiers
                        |> Dict.values
                        |> List.map
                            -- rebuild the identifier compare keys
                            (\i ->
                                if
                                    i.name
                                        == old.name
                                        && ((case i.what of
                                                Type.TType tt ->
                                                    T.find (allTyped state tt) i.identifiable
                                                        |> Maybe.map (\t -> containsScope (allTyped state tt) Dict.empty (IsItem (Type.TType tt) t.uuid) new.scope)

                                                Type.HType ht ->
                                                    H.find (allHierarchic state ht) i.identifiable
                                                        |> Maybe.map (\h -> containsScope Dict.empty (allHierarchic state ht) (IsItem (Type.HType ht) h.uuid) new.scope)
                                            )
                                                |> Maybe.withDefault False
                                           )
                                then
                                    let
                                        newi =
                                            { i | name = new.name }
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
                            (\_ i ->
                                i.name
                                    /= it.name
                                    || not
                                        ((case i.what of
                                            Type.TType tt ->
                                                T.find (allTyped state tt) i.identifiable
                                                    |> Maybe.map (\t -> containsScope (allTyped state tt) (allHierarchic state (TType.toHierarchic tt)) (IsItem (Type.TType tt) t.uuid) it.scope)

                                            Type.HType ht ->
                                                H.find (allHierarchic state ht) i.identifiable
                                                    |> Maybe.map (\h -> containsScope Dict.empty (allHierarchic state ht) (IsItem (Type.HType ht) h.uuid) it.scope)
                                         )
                                            |> Maybe.withDefault False
                                        )
                            )
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Dict.insert (Uuid.toString b.uuid) b.uuid state.uuids
            }

        AddedValueType it ->
            { state
                | valueTypes = Dict.insert (ValueType.compare it) it state.valueTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Dict.insert (Uuid.toString b.uuid) b.uuid state.uuids
            }

        ChangedValueType new old ->
            { state
                | valueTypes =
                    Dict.insert (ValueType.compare new) new <|
                        Dict.remove (ValueType.compare old) state.valueTypes
                , values =
                    -- change the name of all the values whose name equals the new name of the identifier type
                    state.values
                        |> Dict.values
                        |> List.map
                            -- rebuild the value compare keys
                            (\v ->
                                if
                                    v.name
                                        == old.name
                                        && ((case v.what of
                                                Type.TType tt ->
                                                    T.find (allTyped state tt) v.for
                                                        |> Maybe.map (\t -> containsScope (allTyped state tt) Dict.empty (IsItem (Type.TType tt) v.for) new.scope)

                                                Type.HType ht ->
                                                    H.find (allHierarchic state ht) v.for
                                                        |> Maybe.map (\h -> containsScope Dict.empty (allHierarchic state ht) (IsItem (Type.HType ht) v.for) new.scope)
                                            )
                                                |> Maybe.withDefault False
                                           )
                                then
                                    let
                                        newi =
                                            { v | name = new.name }
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
                            (\_ v ->
                                (v.name /= vt.name)
                                    || not
                                        ((case v.what of
                                            Type.TType tt ->
                                                T.find (allTyped state tt) v.for
                                                    |> Maybe.map (\t -> containsScope (allTyped state tt) Dict.empty (IsItem (Type.TType tt) v.for) vt.scope)

                                            Type.HType ht ->
                                                H.find (allHierarchic state ht) v.for
                                                    |> Maybe.map (\h -> containsScope Dict.empty (allHierarchic state ht) (IsItem (Type.HType ht) v.for) vt.scope)
                                         )
                                            |> Maybe.withDefault False
                                        )
                            )
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = Dict.insert (Uuid.toString b.uuid) b.uuid state.uuids
            }

        AddedValue v ->
            { state
                | values = Dict.insert (Value.compare v) v state.values
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedResourceType rt ->
            { state
                | resourceTypes = insertH rt state.resourceTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedResourceType uuid ->
            { state
                | resourceTypes = removeHUuid uuid state.resourceTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedEventType et ->
            { state
                | eventTypes = insertH et state.eventTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedEventType uuid ->
            { state
                | eventTypes = removeHUuid uuid state.eventTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedAgentType at ->
            { state
                | agentTypes = insertH at state.agentTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedAgentType uuid ->
            { state
                | agentTypes = removeHUuid uuid state.agentTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedCommitmentType cmt ->
            { state
                | commitmentTypes = insertH cmt state.commitmentTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedCommitmentType uuid ->
            { state
                | commitmentTypes = removeHUuid uuid state.commitmentTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedContractType cnt ->
            { state
                | contractTypes = insertH cnt state.contractTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedContractType uuid ->
            { state
                | contractTypes = removeHUuid uuid state.contractTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedProcessType pt ->
            { state
                | processTypes = insertH pt state.processTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedProcessType uuid ->
            { state
                | processTypes = removeHUuid uuid state.processTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedResource rt ->
            { state
                | resources = insertT rt state.resources
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedResource uuid ->
            { state
                | resources = removeTUuid uuid state.resources
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedEvent et ->
            { state
                | events = insertT et state.events
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedEvent uuid ->
            { state
                | events = removeTUuid uuid state.events
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedAgent at ->
            { state
                | agents = insertT at state.agents
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedAgent uuid ->
            { state
                | agents = removeTUuid uuid state.agents
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedCommitment cmt ->
            { state
                | commitments = insertT cmt state.commitments
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedCommitment uuid ->
            { state
                | commitments = removeTUuid uuid state.commitments
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedContract cnt ->
            { state
                | contracts = insertT cnt state.contracts
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedContract uuid ->
            { state
                | contracts = removeTUuid uuid state.contracts
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedProcess pt ->
            { state
                | processes = insertT pt state.processes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedProcess uuid ->
            { state
                | processes = removeTUuid uuid state.processes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        AddedIdentifier ei ->
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
                | groupTypes = insertH gt state.groupTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedGroupType uuid ->
            { state
                | groupTypes = removeHUuid uuid state.groupTypes
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        DefinedGroup group ->
            { state
                | groups = Dict.insert (Uuid.toString group.uuid) group state.groups
                , lastMessageTime = b.when
                , pendingMessages = updatePending (Message b p) state.pendingMessages
                , uuids = insertUuid b.uuid state.uuids
            }

        RemovedGroup uuid ->
            { state
                | groups = removeTUuid uuid state.groups
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


allHierarchic : State -> HType.Type -> Dict String (WithGroups (Hierarchic {}))
allHierarchic s t =
    case t of
        HType.ResourceType ->
            s.resourceTypes

        HType.EventType ->
            s.eventTypes

        HType.AgentType ->
            s.agentTypes

        HType.CommitmentType ->
            s.commitmentTypes |> Dict.map (\_ x -> { what = x.what, uuid = x.uuid, parent = x.parent, identifiers = Dict.empty, values = Dict.empty, providers = x.providers, receivers = x.receivers, flow = x.flow, groups = Dict.empty, display = Dict.empty })

        HType.ContractType ->
            s.contractTypes

        HType.ProcessType ->
            s.processTypes

        HType.GroupType ->
            s.groupTypes


allTfromScope : State -> Scope -> Dict String OnlyTyped
allTfromScope s scope =
    case scope of
        Empty ->
            Dict.empty

        IsItem (Type.TType tt) uuid ->
            allTyped s tt

        IsItem (Type.HType ht) uuid ->
            allTyped s (TType.fromHierarchic ht)

        HasType (Type.TType tt) ->
            allTyped s tt

        HasType (Type.HType ht) ->
            allTyped s (TType.fromHierarchic ht)

        HasUserType _ ht uuid ->
            allTyped s (TType.fromHierarchic ht)

        Identified _ ->
            Dict.empty

        And s1 s2 ->
            Dict.intersect (allTfromScope s s1) (allTfromScope s s2)

        Or s1 s2 ->
            Dict.union (allTfromScope s s1) (allTfromScope s s2)

        Not _ ->
            -- FIXME
            Dict.empty


allHfromScope : State -> Scope -> Dict String (Hierarchic (WithGroups {}))
allHfromScope s scope =
    case scope of
        Empty ->
            Dict.empty

        IsItem (Type.HType ht) uuid ->
            allHierarchic s ht

        IsItem (Type.TType tt) uuid ->
            allHierarchic s (TType.toHierarchic tt)

        HasType (Type.TType tt) ->
            allHierarchic s (TType.toHierarchic tt)

        HasType (Type.HType ht) ->
            allHierarchic s ht

        HasUserType _ ht uuid ->
            allHierarchic s ht

        Identified _ ->
            Dict.empty

        And s1 s2 ->
            Dict.intersect (allHfromScope s s1) (allHfromScope s s2)

        Or s1 s2 ->
            Dict.union (allHfromScope s s1) (allHfromScope s s2)

        Not _ ->
            -- FIXME
            Dict.empty
