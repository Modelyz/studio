port module Message exposing (Connection, Message(..), Metadata, Payload(..), base, compare, decoder, encode, exceptCI, getTime, readMessages, storeMessages, storeMessagesToSend)

import Agent.Agent as Agent exposing (Agent)
import AgentType.AgentType as AgentType exposing (AgentType)
import Commitment.Commitment as Commitment exposing (Commitment)
import CommitmentType.CommitmentType as CommitmentType exposing (CommitmentType)
import Configuration exposing (Configuration)
import Contract.Contract as Contract exposing (Contract)
import ContractType.ContractType as ContractType exposing (ContractType)
import Dict exposing (Dict)
import Event.Event as Event exposing (Event)
import EventType.EventType as EventType exposing (EventType)
import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable exposing (Groupable)
import GroupType.GroupType as GroupType exposing (GroupType)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType as IdentifierType exposing (IdentifierType)
import Json.Decode as Decode exposing (Decoder, andThen)
import Json.Encode as Encode
import MessageFlow exposing (MessageFlow)
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Process as Process exposing (Process)
import ProcessType.ProcessType as ProcessType exposing (ProcessType)
import Resource.Resource as Resource exposing (Resource)
import ResourceType.ResourceType as ResourceType exposing (ResourceType)
import Time exposing (millisToPosix, posixToMillis)
import Value.Value as Value exposing (Value)
import Value.ValueType as ValueType exposing (ValueType)



-- read messages from IDB


port readMessages : Encode.Value -> Cmd msg



-- store messages to IDB then send to WS


port storeMessages : Encode.Value -> Cmd msg



-- only store to IDB


port storeMessagesToSend : Encode.Value -> Cmd msg



-- application/user messages --


type alias Metadata =
    { uuid : Uuid
    , when : Time.Posix
    , flow : MessageFlow
    }


type Message
    = Message Metadata Payload


type Payload
    = ConnectionInitiated Connection
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
    | AddedResource Resource
    | RemovedResource Uuid
    | AddedEvent Event
    | RemovedEvent Uuid
    | AddedAgent Agent
    | RemovedAgent Uuid
    | AddedCommitment Commitment
    | RemovedCommitment Uuid
    | AddedContract Contract
    | RemovedContract Uuid
    | AddedProcess Process
    | RemovedProcess Uuid
    | IdentifierTypeAdded IdentifierType
    | IdentifierTypeChanged IdentifierType IdentifierType
    | IdentifierTypeRemoved IdentifierType
    | IdentifierAdded Identifier
    | ValueTypeAdded ValueType
    | ValueTypeChanged ValueType ValueType
    | ValueTypeRemoved ValueType
    | ValueAdded Value
    | Configured Configuration
    | Unconfigured Configuration
    | AddedGroupType GroupType
    | RemovedGroupType Uuid
    | DefinedGroup Group
    | RemovedGroup Uuid
    | Grouped Groupable Group
    | Ungrouped Groupable Group


toString : Payload -> String
toString p =
    case p of
        ConnectionInitiated _ ->
            "ConnectionInitiated"

        IdentifierTypeAdded _ ->
            "IdentifierTypeAdded"

        IdentifierTypeChanged _ _ ->
            "IdentifierTypeChanged"

        IdentifierTypeRemoved _ ->
            "IdentifierTypeRemoved"

        ValueTypeAdded _ ->
            "ValueTypeAdded"

        ValueTypeChanged _ _ ->
            "ValueTypeChanged"

        ValueTypeRemoved _ ->
            "ValueTypeRemoved"

        ValueAdded _ ->
            "ValueAdded"

        AddedResourceType _ ->
            "AddedResourceType"

        RemovedResourceType _ ->
            "RemovedResourceType"

        AddedEventType _ ->
            "AddedEventType"

        RemovedEventType _ ->
            "RemovedEventType"

        AddedAgentType _ ->
            "AddedAgentType"

        RemovedAgentType _ ->
            "RemovedAgentType"

        AddedCommitmentType _ ->
            "AddedCommitmentType"

        RemovedCommitmentType _ ->
            "RemovedCommitmentType"

        AddedContractType _ ->
            "AddedContractType"

        RemovedContractType _ ->
            "RemovedContractType"

        AddedProcessType _ ->
            "AddedProcessType"

        RemovedProcessType _ ->
            "RemovedProcessType"

        AddedResource _ ->
            "AddedResource"

        RemovedResource _ ->
            "RemovedResource"

        AddedEvent _ ->
            "AddedEvent"

        RemovedEvent _ ->
            "RemovedEvent"

        AddedAgent _ ->
            "AddedAgent"

        RemovedAgent _ ->
            "RemovedAgent"

        AddedCommitment _ ->
            "AddedCommitment"

        RemovedCommitment _ ->
            "RemovedCommitment"

        AddedContract _ ->
            "AddedContract"

        RemovedContract _ ->
            "RemovedContract"

        AddedProcess _ ->
            "AddedProcess"

        RemovedProcess _ ->
            "RemovedProcess"

        IdentifierAdded _ ->
            "IdentifierAdded"

        Configured _ ->
            "Configured"

        Unconfigured _ ->
            "Unconfigured"

        AddedGroupType _ ->
            "AddedGroupType"

        RemovedGroupType _ ->
            "RemovedGroupType"

        DefinedGroup _ ->
            "DefinedGroup"

        RemovedGroup _ ->
            "RemovedGroup"

        Grouped _ _ ->
            "Grouped"

        Ungrouped _ _ ->
            "Ungrouped"


type alias Connection =
    { lastMessageTime : Time.Posix, uuids : Dict String Uuid }


base : Message -> Metadata
base (Message b p) =
    b


compare : Message -> Int
compare =
    getTime >> posixToMillis


getTime : Message -> Time.Posix
getTime =
    base >> .when


exceptCI : List Message -> List Message
exceptCI es =
    List.filter
        (\(Message _ p) ->
            case p of
                ConnectionInitiated _ ->
                    False

                _ ->
                    True
        )
        es



-- JSON encoding / decoding


encodeBase : Metadata -> Encode.Value
encodeBase b =
    Encode.object
        [ ( "uuid", Uuid.encode b.uuid )
        , ( "when", Encode.int <| posixToMillis b.when )
        , ( "flow", MessageFlow.encode b.flow )
        ]


encode : Message -> Encode.Value
encode (Message b p) =
    Encode.object
        [ ( "what", Encode.string <| toString p )
        , ( "meta", encodeBase b )
        , case p of
            ConnectionInitiated e ->
                ( "load"
                , Encode.object
                    [ ( "lastMessageTime", Encode.int <| posixToMillis e.lastMessageTime )
                    , ( "uuids", Encode.list Uuid.encode (Dict.values e.uuids) )
                    ]
                )

            IdentifierTypeChanged new old ->
                ( "load"
                , Encode.object
                    [ ( "new", IdentifierType.encode new )
                    , ( "old", IdentifierType.encode old )
                    ]
                )

            IdentifierTypeAdded it ->
                ( "load", IdentifierType.encode it )

            IdentifierTypeRemoved it ->
                ( "load", IdentifierType.encode it )

            ValueTypeAdded vt ->
                ( "load", ValueType.encode vt )

            ValueTypeChanged new old ->
                ( "load"
                , Encode.object
                    [ ( "new", ValueType.encode new )
                    , ( "old", ValueType.encode old )
                    ]
                )

            ValueTypeRemoved vt ->
                ( "load", ValueType.encode vt )

            ValueAdded v ->
                ( "load", Value.encode v )

            AddedResourceType e ->
                ( "load", ResourceType.encode e )

            RemovedResourceType uuid ->
                ( "load", Uuid.encode uuid )

            AddedEventType e ->
                ( "load", EventType.encode e )

            RemovedEventType uuid ->
                ( "load", Uuid.encode uuid )

            AddedAgentType e ->
                ( "load", AgentType.encode e )

            RemovedAgentType e ->
                ( "load", Uuid.encode e )

            AddedCommitmentType e ->
                ( "load", CommitmentType.encode e )

            RemovedCommitmentType uuid ->
                ( "load", Uuid.encode uuid )

            AddedContractType e ->
                ( "load", ContractType.encode e )

            RemovedContractType uuid ->
                ( "load", Uuid.encode uuid )

            AddedProcessType e ->
                ( "load", ProcessType.encode e )

            RemovedProcessType uuid ->
                ( "load", Uuid.encode uuid )

            AddedResource e ->
                ( "load", Resource.encode e )

            RemovedResource uuid ->
                ( "load", Uuid.encode uuid )

            AddedEvent e ->
                ( "load", Event.encode e )

            RemovedEvent uuid ->
                ( "load", Uuid.encode uuid )

            AddedAgent e ->
                ( "load", Agent.encode e )

            RemovedAgent uuid ->
                ( "load", Uuid.encode uuid )

            AddedCommitment e ->
                ( "load", Commitment.encode e )

            RemovedCommitment uuid ->
                ( "load", Uuid.encode uuid )

            AddedContract e ->
                ( "load", Contract.encode e )

            RemovedContract uuid ->
                ( "load", Uuid.encode uuid )

            AddedProcess e ->
                ( "load", Process.encode e )

            RemovedProcess uuid ->
                ( "load", Uuid.encode uuid )

            IdentifierAdded i ->
                ( "load", Identifier.encode i )

            Configured c ->
                ( "load", Configuration.encode c )

            Unconfigured c ->
                ( "load", Configuration.encode c )

            AddedGroupType gt ->
                ( "load", GroupType.encode gt )

            RemovedGroupType uuid ->
                ( "load", Uuid.encode uuid )

            DefinedGroup g ->
                ( "load", Group.encode g )

            RemovedGroup uuid ->
                ( "load", Uuid.encode uuid )

            Grouped e g ->
                ( "load", Encode.object [ ( "groupable", Groupable.encode e ), ( "group", Group.encode g ) ] )

            Ungrouped e g ->
                ( "load", Encode.object [ ( "groupable", Groupable.encode e ), ( "group", Group.encode g ) ] )
        ]


toPosix : Int -> Decoder Time.Posix
toPosix t =
    Decode.succeed (millisToPosix t)


baseDecoder : Decoder Metadata
baseDecoder =
    Decode.map3 Metadata
        (Decode.field "uuid" Uuid.decoder)
        (Decode.field "when" Decode.int |> andThen toPosix)
        (Decode.field "flow" MessageFlow.decoder)


decoder : Decoder Message
decoder =
    Decode.map2 Message
        (Decode.field "meta" baseDecoder)
        (Decode.field "what" Decode.string
            |> andThen
                (\t ->
                    case t of
                        "ConnectionInitiated" ->
                            Decode.map ConnectionInitiated
                                (Decode.field "load"
                                    (Decode.map2 Connection
                                        (Decode.field "lastMessageTime" Decode.int |> andThen toPosix)
                                        (Decode.field "uuids" (Decode.list Uuid.decoder) |> andThen (List.map (\u -> ( Uuid.toString u, u )) >> Dict.fromList >> Decode.succeed))
                                    )
                                )

                        "IdentifierTypeAdded" ->
                            Decode.map IdentifierTypeAdded
                                (Decode.field "load" IdentifierType.decoder)

                        "IdentifierTypeChanged" ->
                            Decode.map2 IdentifierTypeChanged
                                (Decode.at [ "load", "new" ] IdentifierType.decoder)
                                (Decode.at [ "load", "old" ] IdentifierType.decoder)

                        "IdentifierTypeRemoved" ->
                            Decode.map IdentifierTypeRemoved
                                (Decode.field "load" IdentifierType.decoder)

                        "ValueTypeAdded" ->
                            Decode.map ValueTypeAdded
                                (Decode.field "load" ValueType.decoder)

                        "ValueTypeChanged" ->
                            Decode.map2 ValueTypeChanged
                                (Decode.at [ "load", "new" ] ValueType.decoder)
                                (Decode.at [ "load", "old" ] ValueType.decoder)

                        "ValueTypeRemoved" ->
                            Decode.map ValueTypeRemoved
                                (Decode.field "load" ValueType.decoder)

                        "ValueAdded" ->
                            Decode.map ValueAdded
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
                            Decode.map RemovedProcessType
                                (Decode.field "load" Uuid.decoder)

                        "IdentifierAdded" ->
                            Decode.map IdentifierAdded
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
                            Decode.map2 Grouped (Decode.at [ "load", "groupable" ] Groupable.decoder) (Decode.at [ "load", "group" ] Group.decoder)

                        "Ungrouped" ->
                            Decode.map2 Ungrouped (Decode.at [ "load", "groupable" ] Groupable.decoder) (Decode.at [ "load", "group" ] Group.decoder)

                        _ ->
                            Decode.fail <| "Unknown Message type: " ++ t
                )
        )
