port module Message exposing (Message(..), Metadata, Payload(..), base, compare, decodelist, decoder, encode, exceptCI, getTime, readMessages, storeMessages, storeMessagesToSend)

import Agent.Agent as Agent exposing (Agent)
import AgentType.AgentType as AgentType exposing (AgentType)
import Commitment.Commitment as Commitment exposing (Commitment)
import CommitmentType.CommitmentType as CommitmentType exposing (CommitmentType)
import Configuration exposing (Configuration)
import Contract.Contract as Contract exposing (Contract)
import ContractType.ContractType as ContractType exposing (ContractType)
import DictSet as Set
import Event.Event as Event exposing (Event)
import EventType.EventType as EventType exposing (EventType)
import Group.Group as Group exposing (Group)
import Group.Groupable as Groupable exposing (Groupable)
import GroupType.GroupType as GroupType exposing (GroupType)
import Ident.Identifier as Identifier exposing (Identifier)
import Ident.IdentifierType as IdentifierType exposing (IdentifierType)
import Json.Decode as Decode exposing (Decoder, andThen, decodeValue)
import Json.Encode as Encode exposing (Value)
import MessageFlow exposing (MessageFlow, decoder)
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Process as Process exposing (Process)
import ProcessType.ProcessType as ProcessType exposing (ProcessType)
import Resource.Resource as Resource exposing (Resource)
import ResourceType.ResourceType as ResourceType exposing (ResourceType)
import Restriction.Restriction as Restriction exposing (Restriction)
import Time exposing (millisToPosix, posixToMillis)



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
    | RemovedResourceType ResourceType
    | AddedEventType EventType
    | RemovedEventType EventType
    | AddedAgentType AgentType
    | RemovedAgentType AgentType
    | AddedCommitmentType CommitmentType
    | RemovedCommitmentType CommitmentType
    | AddedContractType ContractType
    | RemovedContractType ContractType
    | AddedProcessType ProcessType
    | RemovedProcessType ProcessType
    | AddedResource Resource
    | RemovedResource Resource
    | AddedEvent Event
    | RemovedEvent Event
    | AddedAgent Agent
    | RemovedAgent Agent
    | AddedCommitment Commitment
    | RemovedCommitment Commitment
    | AddedContract Contract
    | RemovedContract Contract
    | AddedProcess Process
    | RemovedProcess Process
    | Restricted Restriction
    | IdentifierTypeAdded IdentifierType
    | IdentifierTypeRemoved IdentifierType
    | IdentifierAdded Identifier
    | Configured Configuration
    | Unconfigured Configuration
    | AddedGroupType GroupType
    | RemovedGroupType GroupType
    | DefinedGroup Group
    | RemovedGroup Group
    | Grouped Groupable Group
    | Ungrouped Groupable Group


toString : Payload -> String
toString p =
    case p of
        ConnectionInitiated _ ->
            "ConnectionInitiated"

        Restricted _ ->
            "Restricted"

        IdentifierTypeAdded _ ->
            "IdentifierTypeAdded"

        IdentifierTypeRemoved _ ->
            "IdentifierTypeRemoved"

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
    -- TODO move in its module?
    { lastMessageTime : Time.Posix, uuids : Set.DictSet String Uuid }


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
        (\(Message b p) ->
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
            Restricted r ->
                ( "load", Restriction.encode r )

            ConnectionInitiated e ->
                ( "load"
                , Encode.object
                    [ ( "lastMessageTime", Encode.int <| posixToMillis e.lastMessageTime )
                    , ( "uuids", Encode.list Uuid.encode <| Set.toList e.uuids )
                    ]
                )

            IdentifierTypeAdded it ->
                ( "load", IdentifierType.encode it )

            IdentifierTypeRemoved it ->
                ( "load", IdentifierType.encode it )

            AddedResourceType e ->
                ( "load", ResourceType.encode e )

            RemovedResourceType e ->
                ( "load", ResourceType.encode e )

            AddedEventType e ->
                ( "load", EventType.encode e )

            RemovedEventType e ->
                ( "load", EventType.encode e )

            AddedAgentType e ->
                ( "load", AgentType.encode e )

            RemovedAgentType e ->
                ( "load", AgentType.encode e )

            AddedCommitmentType e ->
                ( "load", CommitmentType.encode e )

            RemovedCommitmentType e ->
                ( "load", CommitmentType.encode e )

            AddedContractType e ->
                ( "load", ContractType.encode e )

            RemovedContractType e ->
                ( "load", ContractType.encode e )

            AddedProcessType e ->
                ( "load", ProcessType.encode e )

            RemovedProcessType e ->
                ( "load", ProcessType.encode e )

            AddedResource e ->
                ( "load", Resource.encode e )

            RemovedResource e ->
                ( "load", Resource.encode e )

            AddedEvent e ->
                ( "load", Event.encode e )

            RemovedEvent e ->
                ( "load", Event.encode e )

            AddedAgent e ->
                ( "load", Agent.encode e )

            RemovedAgent e ->
                ( "load", Agent.encode e )

            AddedCommitment e ->
                ( "load", Commitment.encode e )

            RemovedCommitment e ->
                ( "load", Commitment.encode e )

            AddedContract e ->
                ( "load", Contract.encode e )

            RemovedContract e ->
                ( "load", Contract.encode e )

            AddedProcess e ->
                ( "load", Process.encode e )

            RemovedProcess e ->
                ( "load", Process.encode e )

            IdentifierAdded i ->
                ( "load", Identifier.encode i )

            Configured c ->
                ( "load", Configuration.encode c )

            Unconfigured c ->
                ( "load", Configuration.encode c )

            AddedGroupType gt ->
                ( "load", GroupType.encode gt )

            RemovedGroupType gt ->
                ( "load", GroupType.encode gt )

            DefinedGroup g ->
                ( "load", Group.encode g )

            RemovedGroup g ->
                ( "load", Group.encode g )

            Grouped e g ->
                ( "load", Encode.object [ ( "entity", Groupable.encode e ), ( "group", Group.encode g ) ] )

            Ungrouped e g ->
                ( "load", Encode.object [ ( "entity", Groupable.encode e ), ( "group", Group.encode g ) ] )
        ]


decodelist : Decode.Value -> List Message
decodelist =
    Result.withDefault [] << decodeValue (Decode.list decoder)


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
                        "Restricted" ->
                            Decode.map Restricted
                                (Decode.field "load" Restriction.decoder)

                        "ConnectionInitiated" ->
                            Decode.map ConnectionInitiated
                                (Decode.field "load"
                                    (Decode.map2 Connection
                                        (Decode.field "lastMessageTime" Decode.int |> andThen toPosix)
                                        (Decode.field "uuids" (Decode.list Uuid.decoder) |> andThen (\xs -> Decode.succeed (Set.fromList Uuid.toString xs)))
                                    )
                                )

                        "IdentifierTypeAdded" ->
                            Decode.map IdentifierTypeAdded
                                (Decode.field "load" IdentifierType.decoder)

                        "IdentifierTypeRemoved" ->
                            Decode.map IdentifierTypeRemoved
                                (Decode.field "load" IdentifierType.decoder)

                        "AddedResourceType" ->
                            Decode.map AddedResourceType
                                (Decode.field "load" ResourceType.decoder)

                        "RemovedResourceType" ->
                            Decode.map RemovedResourceType
                                (Decode.field "load" ResourceType.decoder)

                        "AddedEventType" ->
                            Decode.map AddedEventType
                                (Decode.field "load" EventType.decoder)

                        "RemovedEventType" ->
                            Decode.map RemovedEventType
                                (Decode.field "load" EventType.decoder)

                        "AddedAgentType" ->
                            Decode.map AddedAgentType
                                (Decode.field "load" AgentType.decoder)

                        "RemovedAgentType" ->
                            Decode.map RemovedAgentType
                                (Decode.field "load" AgentType.decoder)

                        "AddedCommitmentType" ->
                            Decode.map AddedCommitmentType
                                (Decode.field "load" CommitmentType.decoder)

                        "RemovedCommitmentType" ->
                            Decode.map RemovedCommitmentType
                                (Decode.field "load" CommitmentType.decoder)

                        "AddedContractType" ->
                            Decode.map AddedContractType
                                (Decode.field "load" ContractType.decoder)

                        "RemovedContractType" ->
                            Decode.map RemovedContractType
                                (Decode.field "load" ContractType.decoder)

                        "AddedProcessType" ->
                            Decode.map AddedProcessType
                                (Decode.field "load" ProcessType.decoder)

                        "RemovedProcessType" ->
                            Decode.map RemovedProcessType
                                (Decode.field "load" ProcessType.decoder)

                        "AddedResource" ->
                            Decode.map AddedResource
                                (Decode.field "load" Resource.decoder)

                        "RemovedResource" ->
                            Decode.map RemovedResource
                                (Decode.field "load" Resource.decoder)

                        "AddedEvent" ->
                            Decode.map AddedEvent
                                (Decode.field "load" Event.decoder)

                        "RemovedEvent" ->
                            Decode.map RemovedEvent
                                (Decode.field "load" Event.decoder)

                        "AddedAgent" ->
                            Decode.map AddedAgent
                                (Decode.field "load" Agent.decoder)

                        "RemovedAgent" ->
                            Decode.map RemovedAgent
                                (Decode.field "load" Agent.decoder)

                        "AddedCommitment" ->
                            Decode.map AddedCommitment
                                (Decode.field "load" Commitment.decoder)

                        "RemovedCommitment" ->
                            Decode.map RemovedCommitment
                                (Decode.field "load" Commitment.decoder)

                        "AddedContract" ->
                            Decode.map AddedContract
                                (Decode.field "load" Contract.decoder)

                        "RemovedContract" ->
                            Decode.map RemovedContract
                                (Decode.field "load" Contract.decoder)

                        "AddedProcess" ->
                            Decode.map AddedProcess
                                (Decode.field "load" Process.decoder)

                        "RemovedProcess" ->
                            Decode.map RemovedProcessType
                                (Decode.field "load" ProcessType.decoder)

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
                            Decode.map RemovedGroupType (Decode.field "load" GroupType.decoder)

                        "DefinedGroup" ->
                            Decode.map DefinedGroup (Decode.field "load" Group.decoder)

                        "RemovedGroup" ->
                            Decode.map RemovedGroup (Decode.field "load" Group.decoder)

                        "Grouped" ->
                            Decode.map2 Grouped (Decode.at [ "load", "groupable" ] Groupable.decoder) (Decode.at [ "load", "group" ] Group.decoder)

                        "Ungrouped" ->
                            Decode.map2 Ungrouped (Decode.at [ "load", "groupable" ] Groupable.decoder) (Decode.at [ "load", "group" ] Group.decoder)

                        _ ->
                            Decode.fail <| "Unknown Message type: " ++ t
                )
        )
