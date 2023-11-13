module Payload exposing (Payload(..), decoder, encode)

import Agent.Agent as Agent exposing (Agent)
import AgentType.AgentType as AgentType exposing (AgentType)
import Commitment.Commitment as Commitment exposing (Commitment)
import CommitmentType.CommitmentType as CommitmentType exposing (CommitmentType)
import Configuration exposing (Configuration)
import Connection exposing (Connection)
import Contract.Contract as Contract exposing (Contract)
import ContractType.ContractType as ContractType exposing (ContractType)
import Dict
import Event.Event as Event exposing (Event)
import EventType.EventType as EventType exposing (EventType)
import Group.Group as Group exposing (Group)
import Group.Link as GroupLink
import GroupType.GroupType as GroupType exposing (GroupType)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType as IdentifierType exposing (IdentifierType)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import MessageId
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Process as Process exposing (Process)
import Process.Reconcile as Reconcile exposing (Reconciliation)
import ProcessType.ProcessType as ProcessType exposing (ProcessType)
import Resource.Resource as Resource exposing (Resource)
import ResourceType.ResourceType as ResourceType exposing (ResourceType)
import Time exposing (millisToPosix, posixToMillis)
import Value.Value as Value exposing (Value)
import Value.ValueType as ValueType exposing (ValueType)


type Payload
    = Null
    | InitiatedConnection Connection
      -- TYPES (Maybe studio should only provides these)
    | AddedResourceType ResourceType
    | RemovedResourceType Uuid
    | AddedEventType EventType
    | RemovedEventType Uuid
    | AddedAgentType AgentType
    | RemovedAgentType Uuid
    | AddedCommitmentType CommitmentType
    | RemovedCommitmentType Uuid
    | AddedContractType ContractType
    | RemovedContractType Uuid
    | AddedProcessType ProcessType
    | RemovedProcessType Uuid
    | AddedIdentifierType IdentifierType
    | ChangedIdentifierType { old : IdentifierType, new : IdentifierType }
    | RemovedIdentifierType IdentifierType
    | AddedValueType ValueType
    | ChangedValueType { old : ValueType, new : ValueType }
    | RemovedValueType ValueType
    | AddedGroupType GroupType
    | RemovedGroupType Uuid
      -- ENTITIES (Maybe this should be part of a specific UI)
    | AddedResource Resource -- should be removed: we can't create a resource from thin air
    | RemovedResource Uuid -- should be removed: we can't destroy a resource. It is destroyed with en Event
    | AddedEvent Event
    | RemovedEvent Uuid -- Is it necessary? An event cannot be removed (at least in the long term)
    | AddedAgent Agent -- rename : RecordAgent.
    | RemovedAgent Uuid
    | AddedCommitment Commitment
    | RemovedCommitment Uuid
    | AddedContract Contract
    | RemovedContract Uuid
    | AddedProcess Process -- maybe rename to StartedProcess
    | RemovedProcess Uuid -- Can a process be removed ? Rather StopProcess
    | DefinedGroup Group
    | RemovedGroup Uuid
      -- BEHAVIOURS
    | AddedIdentifier Identifier
    | AddedValue Value
    | Configured Configuration
    | Unconfigured Configuration
    | Grouped GroupLink.Link
    | Ungrouped GroupLink.Link
    | Reconciled Reconciliation
    | Unreconciled Reconciliation


encode : Payload -> Encode.Value
encode payload =
    case payload of
        Null ->
            Encode.object [ ( "what", Encode.string "Null" ) ]

        InitiatedConnection c ->
            Encode.object
                [ ( "what", Encode.string "InitiatedConnection" )
                , ( "load"
                  , Encode.object
                        [ ( "lastMessageTime", Encode.int <| posixToMillis c.lastMessageTime )
                        , ( "uuids", Encode.list MessageId.encode (Dict.values c.uuids) )
                        ]
                  )
                ]

        ChangedIdentifierType c ->
            Encode.object
                [ ( "what", Encode.string "ChangedIdentifierType" )
                , ( "load"
                  , Encode.object
                        [ ( "old", IdentifierType.encode c.old )
                        , ( "new", IdentifierType.encode c.new )
                        ]
                  )
                ]

        AddedIdentifierType it ->
            Encode.object
                [ ( "what", Encode.string "AddedIdentifierType" )
                , ( "load", IdentifierType.encode it )
                ]

        RemovedIdentifierType it ->
            Encode.object
                [ ( "what", Encode.string "RemovedIdentifierType" )
                , ( "load", IdentifierType.encode it )
                ]

        AddedValueType vt ->
            Encode.object
                [ ( "what", Encode.string "AddedValueType" )
                , ( "load", ValueType.encode vt )
                ]

        ChangedValueType c ->
            Encode.object
                [ ( "what", Encode.string "ChangedValueType" )
                , ( "load"
                  , Encode.object
                        [ ( "old", ValueType.encode c.old )
                        , ( "new", ValueType.encode c.new )
                        ]
                  )
                ]

        RemovedValueType vt ->
            Encode.object
                [ ( "what", Encode.string "RemovedValueType" )
                , ( "load", ValueType.encode vt )
                ]

        AddedValue v ->
            Encode.object
                [ ( "what", Encode.string "AddedValue" )
                , ( "load", Value.encode v )
                ]

        AddedResourceType e ->
            Encode.object
                [ ( "what", Encode.string "AddedResourceType" )
                , ( "load", ResourceType.encode e )
                ]

        RemovedResourceType uuid ->
            Encode.object
                [ ( "what", Encode.string "RemovedResourceType" )
                , ( "load", Uuid.encode uuid )
                ]

        AddedEventType e ->
            Encode.object
                [ ( "what", Encode.string "AddedEventType" )
                , ( "load", EventType.encode e )
                ]

        RemovedEventType uuid ->
            Encode.object
                [ ( "what", Encode.string "RemovedEventType" )
                , ( "load", Uuid.encode uuid )
                ]

        AddedAgentType e ->
            Encode.object
                [ ( "what", Encode.string "AddedAgentType" )
                , ( "load", AgentType.encode e )
                ]

        RemovedAgentType e ->
            Encode.object
                [ ( "what", Encode.string "RemovedAgentType" )
                , ( "load", Uuid.encode e )
                ]

        AddedCommitmentType e ->
            Encode.object
                [ ( "what", Encode.string "AddedCommitmentType" )
                , ( "load", CommitmentType.encode e )
                ]

        RemovedCommitmentType uuid ->
            Encode.object
                [ ( "what", Encode.string "RemovedCommitmentType" )
                , ( "load", Uuid.encode uuid )
                ]

        AddedContractType e ->
            Encode.object
                [ ( "what", Encode.string "AddedContractType" )
                , ( "load", ContractType.encode e )
                ]

        RemovedContractType uuid ->
            Encode.object
                [ ( "what", Encode.string "RemovedContractType" )
                , ( "load", Uuid.encode uuid )
                ]

        AddedProcessType e ->
            Encode.object
                [ ( "what", Encode.string "AddedProcessType" )
                , ( "load", ProcessType.encode e )
                ]

        RemovedProcessType uuid ->
            Encode.object
                [ ( "what", Encode.string "RemovedProcessType" )
                , ( "load", Uuid.encode uuid )
                ]

        AddedResource e ->
            Encode.object
                [ ( "what", Encode.string "AddedResource" )
                , ( "load", Resource.encode e )
                ]

        RemovedResource uuid ->
            Encode.object
                [ ( "what", Encode.string "RemovedResource" )
                , ( "load", Uuid.encode uuid )
                ]

        AddedEvent e ->
            Encode.object
                [ ( "what", Encode.string "AddedEvent" )
                , ( "load", Event.encode e )
                ]

        RemovedEvent uuid ->
            Encode.object
                [ ( "what", Encode.string "RemovedEvent" )
                , ( "load", Uuid.encode uuid )
                ]

        AddedAgent e ->
            Encode.object
                [ ( "what", Encode.string "AddedAgent" )
                , ( "load", Agent.encode e )
                ]

        RemovedAgent uuid ->
            Encode.object
                [ ( "what", Encode.string "RemovedAgent" )
                , ( "load", Uuid.encode uuid )
                ]

        AddedCommitment e ->
            Encode.object
                [ ( "what", Encode.string "AddedCommitment" )
                , ( "load", Commitment.encode e )
                ]

        RemovedCommitment uuid ->
            Encode.object
                [ ( "what", Encode.string "RemovedCommitment" )
                , ( "load", Uuid.encode uuid )
                ]

        AddedContract e ->
            Encode.object
                [ ( "what", Encode.string "AddedContract" )
                , ( "load", Contract.encode e )
                ]

        RemovedContract uuid ->
            Encode.object
                [ ( "what", Encode.string "RemovedContract" )
                , ( "load", Uuid.encode uuid )
                ]

        AddedProcess e ->
            Encode.object
                [ ( "what", Encode.string "AddedProcess" )
                , ( "load", Process.encode e )
                ]

        RemovedProcess uuid ->
            Encode.object
                [ ( "what", Encode.string "RemovedProcess" )
                , ( "load", Uuid.encode uuid )
                ]

        AddedIdentifier i ->
            Encode.object
                [ ( "what", Encode.string "AddedIdentifier" )
                , ( "load", Identifier.encode i )
                ]

        Configured c ->
            Encode.object
                [ ( "what", Encode.string "Configured" )
                , ( "load", Configuration.encode c )
                ]

        Unconfigured c ->
            Encode.object
                [ ( "what", Encode.string "Unconfigured" )
                , ( "load", Configuration.encode c )
                ]

        AddedGroupType gt ->
            Encode.object
                [ ( "what", Encode.string "AddedGroupType" )
                , ( "load", GroupType.encode gt )
                ]

        RemovedGroupType uuid ->
            Encode.object
                [ ( "what", Encode.string "RemovedGroupType" )
                , ( "load", Uuid.encode uuid )
                ]

        DefinedGroup g ->
            Encode.object
                [ ( "what", Encode.string "DefinedGroup" )
                , ( "load", Group.encode g )
                ]

        RemovedGroup uuid ->
            Encode.object
                [ ( "what", Encode.string "RemovedGroup" )
                , ( "load", Uuid.encode uuid )
                ]

        Grouped link ->
            Encode.object
                [ ( "what", Encode.string "Grouped" )
                , ( "load", GroupLink.encode link )
                ]

        Ungrouped link ->
            Encode.object
                [ ( "what", Encode.string "Ungrouped" )
                , ( "load", GroupLink.encode link )
                ]

        Reconciled reconciliation ->
            Encode.object
                [ ( "what", Encode.string "Reconciled" )
                , ( "load", Reconcile.encode reconciliation )
                ]

        Unreconciled reconciliation ->
            Encode.object
                [ ( "what", Encode.string "Unreconciled" )
                , ( "load", Reconcile.encode reconciliation )
                ]


decoder : Decoder Payload
decoder =
    Decode.field "what" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "Null" ->
                        Decode.succeed Null

                    "InitiatedConnection" ->
                        Decode.map InitiatedConnection
                            (Decode.field "load"
                                (Decode.map2 Connection
                                    (Decode.field "lastMessageTime" Decode.int |> Decode.andThen (millisToPosix >> Decode.succeed))
                                    (Decode.field "uuids" (Decode.list MessageId.decoder)
                                        |> Decode.andThen (\l -> Decode.succeed (List.map (\m -> ( MessageId.compare m, m )) l))
                                        |> Decode.andThen (\l -> Decode.succeed (Dict.fromList l))
                                    )
                                )
                            )

                    "AddedIdentifierType" ->
                        Decode.map AddedIdentifierType
                            (Decode.field "load" IdentifierType.decoder)

                    "ChangedIdentifierType" ->
                        Decode.map2 (\old new -> ChangedIdentifierType { old = old, new = new })
                            (Decode.at [ "load", "new" ] IdentifierType.decoder)
                            (Decode.at [ "load", "old" ] IdentifierType.decoder)

                    "RemovedIdentifierType" ->
                        Decode.map RemovedIdentifierType
                            (Decode.field "load" IdentifierType.decoder)

                    "AddedValueType" ->
                        Decode.map AddedValueType
                            (Decode.field "load" ValueType.decoder)

                    "ChangedValueType" ->
                        Decode.map2 (\old new -> ChangedValueType { old = old, new = new })
                            (Decode.at [ "load", "new" ] ValueType.decoder)
                            (Decode.at [ "load", "old" ] ValueType.decoder)

                    "RemovedValueType" ->
                        Decode.map RemovedValueType
                            (Decode.field "load" ValueType.decoder)

                    "AddedValue" ->
                        Decode.map AddedValue
                            (Decode.field "load" Value.decoder)

                    "AddedResourceType" ->
                        Decode.map AddedResourceType
                            (Decode.field "load" ResourceType.decoder)

                    "RemovedResourceType" ->
                        Decode.map RemovedResourceType
                            (Decode.field "load" Uuid.decoder)

                    "AddedEventType" ->
                        Decode.map AddedEventType
                            (Decode.field "load" EventType.decoder)

                    "RemovedEventType" ->
                        Decode.map RemovedEventType
                            (Decode.field "load" Uuid.decoder)

                    "AddedAgentType" ->
                        Decode.map AddedAgentType
                            (Decode.field "load" AgentType.decoder)

                    "RemovedAgentType" ->
                        Decode.map RemovedAgentType
                            (Decode.field "load" Uuid.decoder)

                    "AddedCommitmentType" ->
                        Decode.map AddedCommitmentType
                            (Decode.field "load" CommitmentType.decoder)

                    "RemovedCommitmentType" ->
                        Decode.map RemovedCommitmentType
                            (Decode.field "load" Uuid.decoder)

                    "AddedContractType" ->
                        Decode.map AddedContractType
                            (Decode.field "load" ContractType.decoder)

                    "RemovedContractType" ->
                        Decode.map RemovedContractType
                            (Decode.field "load" Uuid.decoder)

                    "AddedProcessType" ->
                        Decode.map AddedProcessType
                            (Decode.field "load" ProcessType.decoder)

                    "RemovedProcessType" ->
                        Decode.map RemovedProcessType
                            (Decode.field "load" Uuid.decoder)

                    "AddedResource" ->
                        Decode.map AddedResource
                            (Decode.field "load" Resource.decoder)

                    "RemovedResource" ->
                        Decode.map RemovedResource
                            (Decode.field "load" Uuid.decoder)

                    "AddedEvent" ->
                        Decode.map AddedEvent
                            (Decode.field "load" Event.decoder)

                    "RemovedEvent" ->
                        Decode.map RemovedEvent
                            (Decode.field "load" Uuid.decoder)

                    "AddedAgent" ->
                        Decode.map AddedAgent
                            (Decode.field "load" Agent.decoder)

                    "RemovedAgent" ->
                        Decode.map RemovedAgent
                            (Decode.field "load" Uuid.decoder)

                    "AddedCommitment" ->
                        Decode.map AddedCommitment
                            (Decode.field "load" Commitment.decoder)

                    "RemovedCommitment" ->
                        Decode.map RemovedCommitment
                            (Decode.field "load" Uuid.decoder)

                    "AddedContract" ->
                        Decode.map AddedContract
                            (Decode.field "load" Contract.decoder)

                    "RemovedContract" ->
                        Decode.map RemovedContract
                            (Decode.field "load" Uuid.decoder)

                    "AddedProcess" ->
                        Decode.map AddedProcess
                            (Decode.field "load" Process.decoder)

                    "RemovedProcess" ->
                        Decode.map RemovedProcess
                            (Decode.field "load" Uuid.decoder)

                    "AddedIdentifier" ->
                        Decode.map AddedIdentifier
                            (Decode.field "load" Identifier.decoder)

                    "Configured" ->
                        Decode.map Configured
                            (Decode.field "load" Configuration.decoder)

                    "Unconfigured" ->
                        Decode.map Unconfigured
                            (Decode.field "load" Configuration.decoder)

                    "AddedGroupType" ->
                        Decode.map AddedGroupType (Decode.field "load" GroupType.decoder)

                    "RemovedGroupType" ->
                        Decode.map RemovedGroupType (Decode.field "load" Uuid.decoder)

                    "DefinedGroup" ->
                        Decode.map DefinedGroup (Decode.field "load" Group.decoder)

                    "RemovedGroup" ->
                        Decode.map RemovedGroup (Decode.field "load" Uuid.decoder)

                    "Grouped" ->
                        Decode.map Grouped (Decode.field "load" GroupLink.decoder)

                    "Ungrouped" ->
                        Decode.map Ungrouped (Decode.field "load" GroupLink.decoder)

                    "Reconciled" ->
                        Decode.map Reconciled (Decode.field "load" Reconcile.decoder)

                    "Unreconciled" ->
                        Decode.map Unreconciled (Decode.field "load" Reconcile.decoder)

                    _ ->
                        Decode.fail <| "Unknown Message type: " ++ t
            )
