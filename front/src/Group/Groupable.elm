module Group.Groupable exposing (Groupable(..), compare, decoder, encode, uuid)

import Agent.Agent as Agent exposing (Agent)
import AgentType.AgentType as AgentType exposing (AgentType)
import Commitment.Commitment as Commitment exposing (Commitment)
import CommitmentType.CommitmentType as CommitmentType exposing (CommitmentType)
import Contract.Contract as Contract exposing (Contract)
import ContractType.ContractType as ContractType exposing (ContractType)
import Event.Event as Event exposing (Event)
import EventType.EventType as EventType exposing (EventType)
import Group.Group as Group exposing (Group)
import GroupType.GroupType as GroupType exposing (GroupType)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Prng.Uuid as Uuid exposing (Uuid)
import Process.Process as Process exposing (Process)
import ProcessType.ProcessType as ProcessType exposing (ProcessType)
import Resource.Resource as Resource exposing (Resource)
import ResourceType.ResourceType as ResourceType exposing (ResourceType)


type Groupable
    = R Resource
    | E Event
    | A Agent
    | Cm Commitment
    | Cn Contract
    | P Process
    | G Group
    | RT ResourceType
    | ET EventType
    | AT AgentType
    | CmT CommitmentType
    | CnT ContractType
    | PT ProcessType
    | GT GroupType


uuid : Groupable -> Uuid
uuid x =
    case x of
        R r ->
            r.uuid

        E e ->
            e.uuid

        A a ->
            a.uuid

        Cm cm ->
            cm.uuid

        Cn cn ->
            cn.uuid

        P p ->
            p.uuid

        G g ->
            g.uuid

        RT rt ->
            rt.uuid

        ET et ->
            et.uuid

        AT at ->
            at.uuid

        CmT cmt ->
            cmt.uuid

        CnT cnt ->
            cnt.uuid

        PT pt ->
            pt.uuid

        GT gt ->
            gt.uuid


encode : Groupable -> Encode.Value
encode groupable =
    case groupable of
        R r ->
            Encode.object
                [ ( "what", Encode.string "Resource" )
                , ( "value", Resource.encode r )
                ]

        E e ->
            Encode.object
                [ ( "what", Encode.string "Event" )
                , ( "value", Event.encode e )
                ]

        A a ->
            Encode.object
                [ ( "what", Encode.string "Agent" )
                , ( "value", Agent.encode a )
                ]

        Cm cm ->
            Encode.object
                [ ( "what", Encode.string "Commitment" )
                , ( "value", Commitment.encode cm )
                ]

        Cn cn ->
            Encode.object
                [ ( "what", Encode.string "Contract" )
                , ( "value", Contract.encode cn )
                ]

        P p ->
            Encode.object
                [ ( "what", Encode.string "Process" )
                , ( "value", Process.encode p )
                ]

        G g ->
            Encode.object
                [ ( "what", Encode.string "Group" )
                , ( "value", Group.encode g )
                ]

        RT rt ->
            Encode.object
                [ ( "what", Encode.string "ResourceType" )
                , ( "value", ResourceType.encode rt )
                ]

        ET et ->
            Encode.object
                [ ( "what", Encode.string "EventType" )
                , ( "value", EventType.encode et )
                ]

        AT at ->
            Encode.object
                [ ( "what", Encode.string "AgentType" )
                , ( "value", AgentType.encode at )
                ]

        CmT cmt ->
            Encode.object
                [ ( "what", Encode.string "CommitmentType" )
                , ( "value", CommitmentType.encode cmt )
                ]

        CnT cnt ->
            Encode.object
                [ ( "what", Encode.string "ContractType" )
                , ( "value", ContractType.encode cnt )
                ]

        PT pt ->
            Encode.object
                [ ( "what", Encode.string "ProcessType" )
                , ( "value", ProcessType.encode pt )
                ]

        GT gt ->
            Encode.object
                [ ( "what", Encode.string "GroupType" )
                , ( "value", GroupType.encode gt )
                ]


decoder : Decoder Groupable
decoder =
    Decode.field "what" Decode.string
        |> Decode.andThen
            (\t ->
                Decode.field "value"
                    (case t of
                        "Resource" ->
                            Decode.map R Resource.decoder

                        "Event" ->
                            Decode.map E Event.decoder

                        "Agent" ->
                            Decode.map A Agent.decoder

                        "Commitment" ->
                            Decode.map Cm Commitment.decoder

                        "Contract" ->
                            Decode.map Cn Contract.decoder

                        "Process" ->
                            Decode.map P Process.decoder

                        "Group" ->
                            Decode.map G Group.decoder

                        "ResourceType" ->
                            Decode.map RT ResourceType.decoder

                        "EventType" ->
                            Decode.map ET EventType.decoder

                        "AgentType" ->
                            Decode.map AT AgentType.decoder

                        "CommitmentType" ->
                            Decode.map CmT CommitmentType.decoder

                        "ContractType" ->
                            Decode.map CnT ContractType.decoder

                        "ProcessType" ->
                            Decode.map PT ProcessType.decoder

                        "GroupType" ->
                            Decode.map GT GroupType.decoder

                        _ ->
                            Decode.fail "Unknown entity"
                    )
            )


compare : Groupable -> String
compare groupable =
    case groupable of
        R r ->
            Resource.compare r

        E e ->
            Event.compare e

        A a ->
            Agent.compare a

        Cm cm ->
            Commitment.compare cm

        Cn cn ->
            Contract.compare cn

        P p ->
            Process.compare p

        G g ->
            Group.compare g

        RT rt ->
            ResourceType.compare rt

        ET et ->
            EventType.compare et

        AT at ->
            AgentType.compare at

        CmT cmt ->
            CommitmentType.compare cmt

        CnT cnt ->
            ContractType.compare cnt

        PT pt ->
            ProcessType.compare pt

        GT gt ->
            GroupType.compare gt
